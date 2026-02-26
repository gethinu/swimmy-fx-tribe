//+------------------------------------------------------------------+
//|                                      InstitutionalHunterEA.mq5   |
//|                                Strict Smart-Money Hunting (MT5)  |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "H4 OrderBlock/MSB + M15 execution with strict institutional filters"

#include <Trade/Trade.mqh>

input string   InpSymbolsCsv = "ALL";        // ALL or comma-separated symbol list
input int      InpMagic = 9102501;
input bool     InpUseTimer = true;
input int      InpTimerSec = 10;

// Timeframes
input ENUM_TIMEFRAMES InpZoneTF = PERIOD_H4;   // Zone detection TF
input ENUM_TIMEFRAMES InpExecTF = PERIOD_M15;  // Execution TF

// Strict filters (institutional proxy)
input int      InpVolumeSmaPeriod = 20;
input int      InpVolumeZPeriod = 50;
input double   InpVolumeSmaMult = 2.0;         // tick_volume >= SMA20 * 2.0
input double   InpVolumeZMin = 2.0;            // z-score >= 2.0
input int      InpATRFastPeriod = 14;
input int      InpATRSlowPeriod = 50;
input double   InpATRExpansionMin = 1.3;       // ATR14 / ATR50 >= 1.3

// Structure / zone params
input int      InpSwingLookback = 5;
input int      InpBreakLookbackBars = 120;
input int      InpObSearchBars = 30;
input double   InpOBTouchBufferPoints = 30.0;

// Risk controls
input double   InpRiskPerTradePct = 0.5;       // 0.5% equity risk per trade
input int      InpATRPeriod = 14;
input double   InpSL_ATR_Mult = 2.0;
input double   InpTP_ATR_Mult = 3.0;
input int      InpMaxOpenPositionsTotal = 3;
input bool     InpOnePositionPerSymbol = true;
input int      InpMaxTradesPerSymbolPerDay = 2; // Daily cap per symbol
input int      InpMinBarsBetweenEntries = 4;    // Cooldown bars on exec timeframe
input double   InpDailyDdStopPct = 2.0;        // stop entries after daily DD >= 2%
input double   InpSLAtrBufferMult = 0.5;       // legacy parameter (kept for .set compatibility)

// Safety / execution tuning
input int      InpSlippagePoints = 30;
input double   InpMaxSpreadPoints = 25.0;      // broker point-based spread gate
input int      InpTradeStartHour = 7;          // server time
input int      InpTradeEndHour = 22;           // server time
input bool     InpVerboseLog = true;

CTrade g_trade;

struct ZoneData {
   bool bullish_valid;
   double bullish_low;
   double bullish_high;
   datetime bullish_time;

   bool bearish_valid;
   double bearish_low;
   double bearish_high;
   datetime bearish_time;

   bool msb_bullish;
   bool msb_bearish;
};

struct SymbolState {
   string symbol;
   datetime last_exec_bar_time;
   datetime last_entry_bar_time;
   int day_key;
   int trades_today;
};

string g_symbols[];
SymbolState g_states[];
double g_day_start_equity = 0.0;
datetime g_day_anchor = 0;

int BuildDayKey(const datetime when_time);
void EnsureSymbolDayState(SymbolState &st, const datetime now_time);

//+------------------------------------------------------------------+
//| Utility                                                           |
//+------------------------------------------------------------------+
void LogInfo(const string msg) {
   Print("[InstitutionalHunter] ", msg);
}

void LogDebug(const string msg) {
   if(InpVerboseLog) {
      Print("[InstitutionalHunter][DEBUG] ", msg);
   }
}

string TrimCopy(string s) {
   string out = s;
   StringTrimLeft(out);
   StringTrimRight(out);
   return out;
}

void ResetDayAnchorIfNeeded() {
   datetime now = TimeCurrent();
   MqlDateTime t;
   TimeToStruct(now, t);
   t.hour = 0;
   t.min = 0;
   t.sec = 0;
   datetime today0 = StructToTime(t);

   if(g_day_anchor != today0 || g_day_start_equity <= 0.0) {
      g_day_anchor = today0;
      g_day_start_equity = AccountInfoDouble(ACCOUNT_EQUITY);
      LogInfo("Daily anchor reset: equity=" + DoubleToString(g_day_start_equity, 2));
   }
}

bool DailyDdStopped() {
   ResetDayAnchorIfNeeded();
   if(g_day_start_equity <= 0.0) return false;

   double eq = AccountInfoDouble(ACCOUNT_EQUITY);
   double dd_pct = 100.0 * (g_day_start_equity - eq) / g_day_start_equity;
   return (dd_pct >= InpDailyDdStopPct);
}

int CountOpenPositionsTotal() {
   int total = 0;
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket == 0) continue;
      if(!PositionSelectByTicket(ticket)) continue;
      if((long)PositionGetInteger(POSITION_MAGIC) != InpMagic) continue;
      total++;
   }
   return total;
}

bool HasOpenPositionForSymbol(const string symbol) {
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket == 0) continue;
      if(!PositionSelectByTicket(ticket)) continue;
      if((long)PositionGetInteger(POSITION_MAGIC) != InpMagic) continue;
      if(PositionGetString(POSITION_SYMBOL) == symbol) return true;
   }
   return false;
}

bool EnsureSymbolSelected(const string symbol) {
   if(SymbolSelect(symbol, true)) return true;
   LogInfo("Failed to select symbol: " + symbol);
   return false;
}

bool IsEntryAllowedByTradeMode(const string symbol, const bool is_buy) {
   long mode = SymbolInfoInteger(symbol, SYMBOL_TRADE_MODE);
   if(mode == SYMBOL_TRADE_MODE_DISABLED) return false;
   if(mode == SYMBOL_TRADE_MODE_CLOSEONLY) return false;
   if(is_buy && mode == SYMBOL_TRADE_MODE_SHORTONLY) return false;
   if(!is_buy && mode == SYMBOL_TRADE_MODE_LONGONLY) return false;
   return true;
}

bool SpreadOK(const string symbol) {
   long spread = 0;
   if(!SymbolInfoInteger(symbol, SYMBOL_SPREAD, spread)) return false;
   return ((double)spread <= InpMaxSpreadPoints);
}

bool TimeOK() {
   MqlDateTime t;
   TimeToStruct(TimeCurrent(), t);

   if(InpTradeStartHour == InpTradeEndHour) return true;

   if(InpTradeStartHour < InpTradeEndHour) {
      return (t.hour >= InpTradeStartHour && t.hour < InpTradeEndHour);
   }

   return (t.hour >= InpTradeStartHour || t.hour < InpTradeEndHour);
}

bool ParseSymbols() {
   ArrayResize(g_symbols, 0);

   string raw = TrimCopy(InpSymbolsCsv);
   string upper = raw;
   StringToUpper(upper);

   if(raw == "" || upper == "ALL") {
      int total = SymbolsTotal(true);
      for(int i = 0; i < total; i++) {
         string sym = SymbolName(i, true);
         if(sym == "") continue;
         if(!EnsureSymbolSelected(sym)) continue;
         int n = ArraySize(g_symbols);
         ArrayResize(g_symbols, n + 1);
         g_symbols[n] = sym;
      }
   } else {
      string parts[];
      int cnt = StringSplit(raw, ',', parts);
      for(int i = 0; i < cnt; i++) {
         string sym = TrimCopy(parts[i]);
         if(sym == "") continue;
         if(!EnsureSymbolSelected(sym)) continue;
         int n = ArraySize(g_symbols);
         ArrayResize(g_symbols, n + 1);
         g_symbols[n] = sym;
      }
   }

   if(ArraySize(g_symbols) == 0) {
      LogInfo("No symbols available.");
      return false;
   }

   ArrayResize(g_states, ArraySize(g_symbols));
   for(int i = 0; i < ArraySize(g_symbols); i++) {
      g_states[i].symbol = g_symbols[i];
      g_states[i].last_exec_bar_time = 0;
      g_states[i].last_entry_bar_time = 0;
      g_states[i].day_key = BuildDayKey(TimeCurrent());
      g_states[i].trades_today = 0;
   }

   return true;
}

int FindStateIndex(const string symbol) {
   for(int i = 0; i < ArraySize(g_states); i++) {
      if(g_states[i].symbol == symbol) return i;
   }
   return -1;
}

int BuildDayKey(const datetime when_time) {
   MqlDateTime ts;
   TimeToStruct(when_time, ts);
   return (ts.year * 1000 + ts.day_of_year);
}

void EnsureSymbolDayState(SymbolState &st, const datetime now_time) {
   int key = BuildDayKey(now_time);
   if(st.day_key != key) {
      st.day_key = key;
      st.trades_today = 0;
   }
}

//+------------------------------------------------------------------+
//| Data helpers                                                      |
//+------------------------------------------------------------------+
bool GetRates(const string symbol,
              ENUM_TIMEFRAMES tf,
              int bars,
              MqlRates &rates[]) {
   ArraySetAsSeries(rates, true);
   int copied = CopyRates(symbol, tf, 0, bars, rates);
   if(copied < bars) return false;
   return true;
}

bool IsSwingHigh(MqlRates &rates[], int idx, int wing) {
   double v = rates[idx].high;
   for(int i = 1; i <= wing; i++) {
      if(rates[idx - i].high >= v) return false;
      if(rates[idx + i].high > v) return false;
   }
   return true;
}

bool IsSwingLow(MqlRates &rates[], int idx, int wing) {
   double v = rates[idx].low;
   for(int i = 1; i <= wing; i++) {
      if(rates[idx - i].low <= v) return false;
      if(rates[idx + i].low < v) return false;
   }
   return true;
}

bool LastClosedBarTime(const string symbol,
                       ENUM_TIMEFRAMES tf,
                       datetime &bar_time) {
   MqlRates r[];
   ArraySetAsSeries(r, true);
   int copied = CopyRates(symbol, tf, 1, 1, r);
   if(copied != 1) return false;
   bar_time = r[0].time;
   return true;
}

double SafePoint(const string symbol) {
   double p = SymbolInfoDouble(symbol, SYMBOL_POINT);
   if(p <= 0.0) p = _Point;
   return p;
}

int NormalizeVolumeDigits(const string symbol, double &lot) {
   double step = SymbolInfoDouble(symbol, SYMBOL_VOLUME_STEP);
   if(step <= 0.0) step = 0.01;

   int digits = 0;
   double v = step;
   while(v < 1.0 && digits < 8) {
      v *= 10.0;
      digits++;
   }
   lot = NormalizeDouble(lot, digits);
   return digits;
}

bool ComputeAtr(const string symbol,
                ENUM_TIMEFRAMES tf,
                int period,
                int shift,
                double &out_val) {
   int handle = iATR(symbol, tf, period);
   if(handle == INVALID_HANDLE) return false;

   double buf[];
   ArraySetAsSeries(buf, true);
   bool ok = (CopyBuffer(handle, 0, shift, 1, buf) == 1);
   IndicatorRelease(handle);
   if(!ok) return false;

   out_val = buf[0];
   return (out_val > 0.0);
}

double GetATR(const string symbol, int period) {
   if(period <= 0) return 0.0;

   int h = iATR(symbol, InpExecTF, period);
   if(h == INVALID_HANDLE) return 0.0;

   double buf[];
   ArraySetAsSeries(buf, true);
   if(CopyBuffer(h, 0, 0, 2, buf) < 2) {
      IndicatorRelease(h);
      return 0.0;
   }

   IndicatorRelease(h);
   return buf[0];
}

bool ComputeVolumeStats(const string symbol,
                        ENUM_TIMEFRAMES tf,
                        int sma_period,
                        int z_period,
                        double &current_vol,
                        double &sma,
                        double &zscore) {
   int need = MathMax(sma_period, z_period) + 2;
   MqlRates rates[];
   if(!GetRates(symbol, tf, need, rates)) return false;

   // Last closed bar is index 1 because series is [0=current forming].
   current_vol = (double)rates[1].tick_volume;

   sma = 0.0;
   for(int i = 1; i <= sma_period; i++) {
      sma += (double)rates[i].tick_volume;
   }
   sma /= (double)sma_period;

   double mean = 0.0;
   for(int i = 1; i <= z_period; i++) {
      mean += (double)rates[i].tick_volume;
   }
   mean /= (double)z_period;

   double var = 0.0;
   for(int i = 1; i <= z_period; i++) {
      double d = (double)rates[i].tick_volume - mean;
      var += d * d;
   }
   var /= MathMax(1, z_period - 1);
   double sd = MathSqrt(var);

   if(sd <= 0.0) return false;
   zscore = (current_vol - mean) / sd;

   return true;
}

bool IsBullishReversalCandle(const MqlRates &bar, const MqlRates &prev) {
   if(bar.close <= bar.open) return false;
   // Simple engulf / rejection style confirmation.
   bool engulf = (bar.open <= prev.close && bar.close >= prev.open);
   bool lower_wick = ((bar.open - bar.low) > (bar.high - bar.close));
   return (engulf || lower_wick);
}

bool IsBearishReversalCandle(const MqlRates &bar, const MqlRates &prev) {
   if(bar.close >= bar.open) return false;
   bool engulf = (bar.open >= prev.close && bar.close <= prev.open);
   bool upper_wick = ((bar.high - bar.open) > (bar.close - bar.low));
   return (engulf || upper_wick);
}

//+------------------------------------------------------------------+
//| Zone detection (H4)                                               |
//+------------------------------------------------------------------+
bool DetectLatestZones(const string symbol, ZoneData &zone) {
   zone.bullish_valid = false;
   zone.bearish_valid = false;
   zone.msb_bullish = false;
   zone.msb_bearish = false;

   int need = MathMax(InpBreakLookbackBars + InpSwingLookback + 20, 200);
   MqlRates rates[];
   if(!GetRates(symbol, InpZoneTF, need, rates)) return false;

   // Find latest swing high / low from closed bars.
   double last_swing_high = 0.0;
   int last_swing_high_idx = -1;
   double last_swing_low = 0.0;
   int last_swing_low_idx = -1;

   for(int i = InpSwingLookback + 2; i < need - InpSwingLookback - 2; i++) {
      if(last_swing_high_idx < 0 && IsSwingHigh(rates, i, InpSwingLookback)) {
         last_swing_high = rates[i].high;
         last_swing_high_idx = i;
      }
      if(last_swing_low_idx < 0 && IsSwingLow(rates, i, InpSwingLookback)) {
         last_swing_low = rates[i].low;
         last_swing_low_idx = i;
      }
      if(last_swing_high_idx >= 0 && last_swing_low_idx >= 0) break;
   }

   if(last_swing_high_idx < 0 || last_swing_low_idx < 0) return false;

   // MSB: close breaks last swing level.
   int break_up_idx = -1;
   int break_dn_idx = -1;
   for(int i = 2; i < MathMin(need - 2, InpBreakLookbackBars); i++) {
      if(break_up_idx < 0 && rates[i].close > last_swing_high) break_up_idx = i;
      if(break_dn_idx < 0 && rates[i].close < last_swing_low) break_dn_idx = i;
      if(break_up_idx >= 0 && break_dn_idx >= 0) break;
   }

   // Bullish OB: last bearish candle before bullish break.
   if(break_up_idx >= 0) {
      for(int j = break_up_idx + 1; j <= MathMin(break_up_idx + InpObSearchBars, need - 2); j++) {
         if(rates[j].close < rates[j].open) {
            zone.bullish_low = rates[j].low;
            zone.bullish_high = rates[j].high;
            zone.bullish_time = rates[j].time;
            zone.bullish_valid = true;
            zone.msb_bullish = true;
            break;
         }
      }
   }

   // Bearish OB: last bullish candle before bearish break.
   if(break_dn_idx >= 0) {
      for(int j = break_dn_idx + 1; j <= MathMin(break_dn_idx + InpObSearchBars, need - 2); j++) {
         if(rates[j].close > rates[j].open) {
            zone.bearish_low = rates[j].low;
            zone.bearish_high = rates[j].high;
            zone.bearish_time = rates[j].time;
            zone.bearish_valid = true;
            zone.msb_bearish = true;
            break;
         }
      }
   }

   return (zone.bullish_valid || zone.bearish_valid);
}

bool PriceTouchesZone(const string symbol,
                     ENUM_TIMEFRAMES tf,
                     double z_low,
                     double z_high,
                     double buffer_points) {
   MqlRates bars[];
   if(!GetRates(symbol, tf, 3, bars)) return false;

   double p = SafePoint(symbol);
   double pad = buffer_points * p;

   // Check last closed candle range intersecting zone (+buffer).
   double low = bars[1].low;
   double high = bars[1].high;
   double zl = z_low - pad;
   double zh = z_high + pad;

   return !(high < zl || low > zh);
}

bool ConfirmExecReversal(const string symbol,
                         bool bullish,
                         bool &ok) {
   ok = false;
   MqlRates bars[];
   if(!GetRates(symbol, InpExecTF, 4, bars)) return false;

   if(bullish) {
      ok = IsBullishReversalCandle(bars[1], bars[2]);
   } else {
      ok = IsBearishReversalCandle(bars[1], bars[2]);
   }
   return true;
}

//+------------------------------------------------------------------+
//| Risk / sizing                                                     |
//+------------------------------------------------------------------+
double CalcLotByRisk(const string symbol, double riskPct, double sl_points) {
   double equity = AccountInfoDouble(ACCOUNT_EQUITY);
   double risk_money = equity * (riskPct / 100.0);

   double tick_value = SymbolInfoDouble(symbol, SYMBOL_TRADE_TICK_VALUE);
   double tick_size = SymbolInfoDouble(symbol, SYMBOL_TRADE_TICK_SIZE);
   if(tick_value <= 0.0 || tick_size <= 0.0 || sl_points <= 0.0) return 0.0;

   double point = SafePoint(symbol);
   if(point <= 0.0) return 0.0;

   double sl_price_distance = sl_points * point;
   double loss_per_lot = (sl_price_distance / tick_size) * tick_value;
   if(loss_per_lot <= 0.0) return 0.0;

   double lot = risk_money / loss_per_lot;
   double minLot = SymbolInfoDouble(symbol, SYMBOL_VOLUME_MIN);
   double maxLot = SymbolInfoDouble(symbol, SYMBOL_VOLUME_MAX);
   double step = SymbolInfoDouble(symbol, SYMBOL_VOLUME_STEP);
   if(minLot <= 0.0 || maxLot <= 0.0) return 0.0;
   if(step <= 0.0) step = 0.01;

   lot = MathMax(minLot, MathMin(maxLot, lot));
   lot = MathFloor(lot / step) * step;
   NormalizeVolumeDigits(symbol, lot);
   if(lot < minLot) return 0.0;

   return lot;
}

//+------------------------------------------------------------------+
//| Signal generation                                                 |
//+------------------------------------------------------------------+
bool PassStrictFilters(const string symbol,
                       double &vol,
                       double &vol_sma,
                       double &vol_z,
                       double &atr14,
                       double &atr50,
                       double &atr_ratio) {
   if(!ComputeVolumeStats(symbol,
                          InpExecTF,
                          InpVolumeSmaPeriod,
                          InpVolumeZPeriod,
                          vol,
                          vol_sma,
                          vol_z)) {
      return false;
   }

   if(!ComputeAtr(symbol, InpExecTF, InpATRFastPeriod, 1, atr14)) return false;
   if(!ComputeAtr(symbol, InpExecTF, InpATRSlowPeriod, 1, atr50)) return false;
   if(atr50 <= 0.0) return false;

   atr_ratio = atr14 / atr50;

   bool volume_ok = (vol >= vol_sma * InpVolumeSmaMult) && (vol_z >= InpVolumeZMin);
   bool atr_ok = (atr_ratio >= InpATRExpansionMin);
   return (volume_ok && atr_ok);
}

bool TryBuildSignal(const string symbol,
                    const ZoneData &zone,
                    bool &is_buy,
                    double &atr14,
                    string &reason) {
   is_buy = false;
   reason = "";

   double vol = 0.0;
   double vol_sma = 0.0;
   double vol_z = 0.0;
   double atr50 = 0.0;
   double atr_ratio = 0.0;

   if(!PassStrictFilters(symbol, vol, vol_sma, vol_z, atr14, atr50, atr_ratio)) {
      reason = "strict filter rejected";
      return false;
   }

   bool touched_bull = false;
   bool touched_bear = false;

   if(zone.bullish_valid) {
      touched_bull = PriceTouchesZone(symbol,
                                      InpExecTF,
                                      zone.bullish_low,
                                      zone.bullish_high,
                                      InpOBTouchBufferPoints);
   }

   if(zone.bearish_valid) {
      touched_bear = PriceTouchesZone(symbol,
                                      InpExecTF,
                                      zone.bearish_low,
                                      zone.bearish_high,
                                      InpOBTouchBufferPoints);
   }

   bool rev_ok = false;

   if(zone.msb_bullish && touched_bull) {
      if(ConfirmExecReversal(symbol, true, rev_ok) && rev_ok) {
         is_buy = true;
         reason = "buy signal: bull msb+ob+strict filters";
         return true;
      }
   }

   if(zone.msb_bearish && touched_bear) {
      if(ConfirmExecReversal(symbol, false, rev_ok) && rev_ok) {
         is_buy = false;
         reason = "sell signal: bear msb+ob+strict filters";
         return true;
      }
   }

   reason = "no zone touch/reversal";
   return false;
}

//+------------------------------------------------------------------+
//| Execution                                                         |
//+------------------------------------------------------------------+
bool SubmitOrder(const string symbol,
                 bool is_buy,
                 double atr14) {
   if(!IsEntryAllowedByTradeMode(symbol, is_buy)) {
      LogDebug(symbol + " trade mode rejected side");
      return false;
   }

   if(!SpreadOK(symbol)) {
      LogDebug(symbol + " spread filter rejected");
      return false;
   }

   double bid = SymbolInfoDouble(symbol, SYMBOL_BID);
   double ask = SymbolInfoDouble(symbol, SYMBOL_ASK);
   if(bid <= 0.0 || ask <= 0.0) return false;

   double entry = is_buy ? ask : bid;
   double atr = atr14;
   if(atr <= 0.0) {
      atr = GetATR(symbol, InpATRPeriod);
   }
   if(atr <= 0.0) {
      LogDebug(symbol + " ATR unavailable");
      return false;
   }

   double point = SafePoint(symbol);
   if(point <= 0.0) return false;

   double sl_points = (InpSL_ATR_Mult * atr) / point;
   double tp_points = (InpTP_ATR_Mult * atr) / point;
   if(sl_points <= 0.0 || tp_points <= 0.0) return false;

   int digits = (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS);
   double sl = 0.0;
   double tp = 0.0;
   if(is_buy) {
      sl = NormalizeDouble(entry - (sl_points * point), digits);
      tp = NormalizeDouble(entry + (tp_points * point), digits);
   } else {
      sl = NormalizeDouble(entry + (sl_points * point), digits);
      tp = NormalizeDouble(entry - (tp_points * point), digits);
   }

   double lot = CalcLotByRisk(symbol, InpRiskPerTradePct, sl_points);
   if(lot <= 0.0) {
      LogDebug(symbol + " lot calculation failed");
      return false;
   }

   g_trade.SetExpertMagicNumber(InpMagic);
   g_trade.SetDeviationInPoints(InpSlippagePoints);

   bool ok = false;
   if(is_buy) {
      ok = g_trade.Buy(lot, symbol, 0.0, sl, tp, "IH-BUY");
   } else {
      ok = g_trade.Sell(lot, symbol, 0.0, sl, tp, "IH-SELL");
   }

   if(!ok) {
      LogInfo(symbol + " order failed retcode=" + IntegerToString((int)g_trade.ResultRetcode()));
      return false;
   }

   LogInfo(symbol + " order submitted " + (is_buy ? "BUY" : "SELL") +
           " lot=" + DoubleToString(lot, 2) +
           " sl=" + DoubleToString(sl, (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS)) +
           " tp=" + DoubleToString(tp, (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS)));
   return true;
}

void ProcessSymbol(const string symbol) {
   if(!TimeOK()) {
      LogDebug("Trading session filter active.");
      return;
   }

   if(DailyDdStopped()) {
      LogDebug("Daily DD stop active. Skip new entries.");
      return;
   }

   if(CountOpenPositionsTotal() >= InpMaxOpenPositionsTotal) {
      LogDebug("Total position cap reached.");
      return;
   }

   if(InpOnePositionPerSymbol && HasOpenPositionForSymbol(symbol)) {
      return;
   }

   int idx = FindStateIndex(symbol);
   if(idx < 0) return;
   EnsureSymbolDayState(g_states[idx], TimeCurrent());

   if(g_states[idx].trades_today >= InpMaxTradesPerSymbolPerDay) {
      LogDebug(symbol + " daily trade cap reached.");
      return;
   }

   datetime last_closed_exec_bar = 0;
   if(!LastClosedBarTime(symbol, InpExecTF, last_closed_exec_bar)) return;
   if(last_closed_exec_bar <= 0) return;

   if(g_states[idx].last_exec_bar_time == last_closed_exec_bar) {
      return; // Evaluate once per execution bar.
   }

   if(InpMinBarsBetweenEntries > 0 && g_states[idx].last_entry_bar_time > 0) {
      int tf_sec = PeriodSeconds(InpExecTF);
      if(tf_sec <= 0) tf_sec = 60;
      int elapsed = (int)(last_closed_exec_bar - g_states[idx].last_entry_bar_time);
      if(elapsed < (InpMinBarsBetweenEntries * tf_sec)) {
         LogDebug(symbol + " cooldown active.");
         return;
      }
   }

   g_states[idx].last_exec_bar_time = last_closed_exec_bar;

   ZoneData zone;
   if(!DetectLatestZones(symbol, zone)) {
      LogDebug(symbol + " zone detection failed");
      return;
   }

   bool is_buy = false;
   double atr14 = 0.0;
   string reason;
   if(!TryBuildSignal(symbol, zone, is_buy, atr14, reason)) {
      LogDebug(symbol + " no signal: " + reason);
      return;
   }

   if(SubmitOrder(symbol, is_buy, atr14)) {
      g_states[idx].last_entry_bar_time = last_closed_exec_bar;
      g_states[idx].trades_today++;
   }
}

void EngineTick() {
   for(int i = 0; i < ArraySize(g_symbols); i++) {
      ProcessSymbol(g_symbols[i]);
   }
}

//+------------------------------------------------------------------+
//| MT5 lifecycle                                                     |
//+------------------------------------------------------------------+
int OnInit() {
   if(!ParseSymbols()) {
      LogInfo("Initialization failed: no tradable symbols");
      return INIT_FAILED;
   }

   g_trade.SetExpertMagicNumber(InpMagic);
   ResetDayAnchorIfNeeded();

   LogInfo("Initialized. symbols=" + IntegerToString(ArraySize(g_symbols)) +
           " zoneTF=" + EnumToString(InpZoneTF) +
           " execTF=" + EnumToString(InpExecTF));

   if(InpUseTimer) {
      EventSetTimer((int)MathMax(1, InpTimerSec));
   }

   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(InpUseTimer) {
      EventKillTimer();
   }
   LogInfo("Deinit reason=" + IntegerToString(reason));
}

void OnTick() {
   if(!InpUseTimer) {
      EngineTick();
   }
}

void OnTimer() {
   if(InpUseTimer) {
      EngineTick();
   }
}
