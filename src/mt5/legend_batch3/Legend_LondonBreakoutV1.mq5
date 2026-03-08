//+------------------------------------------------------------------+
//|                            Legend_LondonBreakoutV1.mq5           |
//|              Batch-3 port from Swimmy LEGEND inventory           |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "LEGEND batch-3: Legend-London-Breakout-V1"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_H1;
input double           InpLots = 0.10;
input ulong            InpMagic = 9307303;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.20;
input double           InpTakeProfitPrice = 0.60;
input int              InpRangeStartHour = 8;
input int              InpRangeEndHour = 9;
input int              InpExitHour = 16;

CTrade g_trade;
datetime g_last_bar_time = 0;
int g_day_key = -1;
double g_session_high = 0.0;
double g_session_low = 0.0;
bool g_range_ready = false;
bool g_traded_today = false;

string TradeSymbol() {
   return (InpSymbol == "" ? _Symbol : InpSymbol);
}

double NormalizePriceValue(const string symbol, const double price) {
   int digits = (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS);
   return NormalizeDouble(price, digits);
}

int BuildDayKey(const datetime when_time) {
   MqlDateTime ts;
   TimeToStruct(when_time, ts);
   return (ts.year * 1000 + ts.day_of_year);
}

bool NewBar() {
   datetime bar_time = iTime(TradeSymbol(), InpTimeframe, 0);
   if(bar_time == 0) return false;
   if(bar_time == g_last_bar_time) return false;
   g_last_bar_time = bar_time;
   return true;
}

bool GetManagedPosition(ulong &ticket, ENUM_POSITION_TYPE &type) {
   string symbol = TradeSymbol();
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong current_ticket = PositionGetTicket(i);
      if(current_ticket == 0) continue;
      if(!PositionSelectByTicket(current_ticket)) continue;
      if(PositionGetString(POSITION_SYMBOL) != symbol) continue;
      if((ulong)PositionGetInteger(POSITION_MAGIC) != InpMagic) continue;
      ticket = current_ticket;
      type = (ENUM_POSITION_TYPE)PositionGetInteger(POSITION_TYPE);
      return true;
   }
   return false;
}

bool LoadSeries(double &close_buf[], double &high_buf[], double &low_buf[]) {
   ArraySetAsSeries(close_buf, true);
   ArraySetAsSeries(high_buf, true);
   ArraySetAsSeries(low_buf, true);
   if(CopyClose(TradeSymbol(), InpTimeframe, 1, 3, close_buf) < 3) return false;
   if(CopyHigh(TradeSymbol(), InpTimeframe, 1, 3, high_buf) < 3) return false;
   if(CopyLow(TradeSymbol(), InpTimeframe, 1, 3, low_buf) < 3) return false;
   return true;
}

void ResetDailyState(const int day_key) {
   g_day_key = day_key;
   g_session_high = 0.0;
   g_session_low = 0.0;
   g_range_ready = false;
   g_traded_today = false;
}

bool CloseManagedPosition() {
   ulong ticket = 0;
   ENUM_POSITION_TYPE type = POSITION_TYPE_BUY;
   if(!GetManagedPosition(ticket, type)) return true;
   return g_trade.PositionClose(ticket);
}

bool OpenManagedPosition(const bool is_buy) {
   string symbol = TradeSymbol();
   MqlTick tick;
   if(!SymbolInfoTick(symbol, tick)) return false;

   double entry = (is_buy ? tick.ask : tick.bid);
   double sl = (is_buy ? entry - InpStopLossPrice : entry + InpStopLossPrice);
   double tp = (is_buy ? entry + InpTakeProfitPrice : entry - InpTakeProfitPrice);
   sl = NormalizePriceValue(symbol, sl);
   tp = NormalizePriceValue(symbol, tp);

   g_trade.SetExpertMagicNumber(InpMagic);
   g_trade.SetDeviationInPoints(InpSlippagePoints);

   bool ok = false;
   if(is_buy) ok = g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "Legend-London-Breakout-V1");
   else ok = g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "Legend-London-Breakout-V1");
   if(ok) g_traded_today = true;
   return ok;
}

int OnInit() {
   if(!SymbolSelect(TradeSymbol(), true)) return INIT_FAILED;
   return INIT_SUCCEEDED;
}

void OnTick() {
   if(!NewBar()) return;

   string symbol = TradeSymbol();
   datetime current_bar_time = iTime(symbol, InpTimeframe, 0);
   datetime last_closed_bar_time = iTime(symbol, InpTimeframe, 1);
   if(current_bar_time == 0 || last_closed_bar_time == 0) return;

   int day_key = BuildDayKey(current_bar_time);
   if(day_key != g_day_key) ResetDailyState(day_key);

   MqlDateTime current_ts;
   MqlDateTime closed_ts;
   TimeToStruct(current_bar_time, current_ts);
   TimeToStruct(last_closed_bar_time, closed_ts);

   double close_buf[];
   double high_buf[];
   double low_buf[];
   if(!LoadSeries(close_buf, high_buf, low_buf)) return;

   if(closed_ts.hour >= InpRangeStartHour && closed_ts.hour < InpRangeEndHour) {
      if(!g_range_ready) {
         g_session_high = high_buf[0];
         g_session_low = low_buf[0];
         g_range_ready = true;
      } else {
         g_session_high = MathMax(g_session_high, high_buf[0]);
         g_session_low = MathMin(g_session_low, low_buf[0]);
      }
   }

   ulong ticket = 0;
   ENUM_POSITION_TYPE type = POSITION_TYPE_BUY;
   bool has_position = GetManagedPosition(ticket, type);

   if(current_ts.hour >= InpExitHour) {
      if(has_position) CloseManagedPosition();
      return;
   }

   if(!g_range_ready || g_traded_today || has_position) return;
   if(current_ts.hour < InpRangeEndHour) return;

   double last_close = close_buf[0];
   if(last_close > g_session_high) {
      OpenManagedPosition(true);
      return;
   }

   if(last_close < g_session_low) {
      OpenManagedPosition(false);
   }
}
