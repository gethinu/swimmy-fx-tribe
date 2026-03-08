//+------------------------------------------------------------------+
//|                        HistS_RecruitRndTrendCross.mq5            |
//|               Historical S extraction batch-1 port               |
//| Original: RECRUIT-RND-1768781166-12                              |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "Historical S batch-1: RECRUIT-RND-1768781166-12"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_D1;
input double           InpLots = 0.01;
input ulong            InpMagic = 9307403;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.034;
input double           InpTakeProfitPrice = 0.043;
input int              InpEntryFastMAPeriod = 5;
input int              InpEntrySlowMAPeriod = 20;
input int              InpExitFastMAPeriod = 13;
input int              InpExitSlowMAPeriod = 120;

CTrade g_trade;
int g_entry_fast_handle = INVALID_HANDLE;
int g_entry_slow_handle = INVALID_HANDLE;
int g_exit_fast_handle = INVALID_HANDLE;
int g_exit_slow_handle = INVALID_HANDLE;
datetime g_last_bar_time = 0;

string TradeSymbol() { return (InpSymbol == "" ? _Symbol : InpSymbol); }

double NormalizePriceValue(const string symbol, const double price) {
   int digits = (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS);
   return NormalizeDouble(price, digits);
}

bool NewBar() {
   datetime bar_time = iTime(TradeSymbol(), InpTimeframe, 0);
   if(bar_time == 0 || bar_time == g_last_bar_time) return false;
   g_last_bar_time = bar_time;
   return true;
}

bool CrossAbove(const double prev_left, const double prev_right,
                const double curr_left, const double curr_right) {
   return (prev_left <= prev_right && curr_left > curr_right);
}

bool CrossBelow(const double prev_left, const double prev_right,
                const double curr_left, const double curr_right) {
   return (prev_left >= prev_right && curr_left < curr_right);
}

bool AnyCross(const double prev_left, const double prev_right,
              const double curr_left, const double curr_right) {
   return CrossAbove(prev_left, prev_right, curr_left, curr_right)
       || CrossBelow(prev_left, prev_right, curr_left, curr_right);
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

bool LoadSeries(double &entry_fast_buf[], double &entry_slow_buf[],
                double &exit_fast_buf[], double &exit_slow_buf[]) {
   ArraySetAsSeries(entry_fast_buf, true);
   ArraySetAsSeries(entry_slow_buf, true);
   ArraySetAsSeries(exit_fast_buf, true);
   ArraySetAsSeries(exit_slow_buf, true);
   if(CopyBuffer(g_entry_fast_handle, 0, 1, 3, entry_fast_buf) < 3) return false;
   if(CopyBuffer(g_entry_slow_handle, 0, 1, 3, entry_slow_buf) < 3) return false;
   if(CopyBuffer(g_exit_fast_handle, 0, 1, 3, exit_fast_buf) < 3) return false;
   if(CopyBuffer(g_exit_slow_handle, 0, 1, 3, exit_slow_buf) < 3) return false;
   return true;
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
   if(is_buy) return g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "RECRUIT-RND-1768781166-12");
   return g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "RECRUIT-RND-1768781166-12");
}

int OnInit() {
   string symbol = TradeSymbol();
   if(!SymbolSelect(symbol, true)) return INIT_FAILED;
   g_entry_fast_handle = iMA(symbol, InpTimeframe, InpEntryFastMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   g_entry_slow_handle = iMA(symbol, InpTimeframe, InpEntrySlowMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   g_exit_fast_handle = iMA(symbol, InpTimeframe, InpExitFastMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   g_exit_slow_handle = iMA(symbol, InpTimeframe, InpExitSlowMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   if(g_entry_fast_handle == INVALID_HANDLE || g_entry_slow_handle == INVALID_HANDLE ||
      g_exit_fast_handle == INVALID_HANDLE || g_exit_slow_handle == INVALID_HANDLE)
      return INIT_FAILED;
   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(g_entry_fast_handle != INVALID_HANDLE) IndicatorRelease(g_entry_fast_handle);
   if(g_entry_slow_handle != INVALID_HANDLE) IndicatorRelease(g_entry_slow_handle);
   if(g_exit_fast_handle != INVALID_HANDLE) IndicatorRelease(g_exit_fast_handle);
   if(g_exit_slow_handle != INVALID_HANDLE) IndicatorRelease(g_exit_slow_handle);
}

void OnTick() {
   if(!NewBar()) return;

   double entry_fast_buf[];
   double entry_slow_buf[];
   double exit_fast_buf[];
   double exit_slow_buf[];
   if(!LoadSeries(entry_fast_buf, entry_slow_buf, exit_fast_buf, exit_slow_buf)) return;

   ulong ticket = 0;
   ENUM_POSITION_TYPE type = POSITION_TYPE_BUY;
   bool has_position = GetManagedPosition(ticket, type);

   bool long_entry = CrossAbove(entry_fast_buf[1], entry_slow_buf[1], entry_fast_buf[0], entry_slow_buf[0]);
   bool short_entry = CrossBelow(entry_fast_buf[1], entry_slow_buf[1], entry_fast_buf[0], entry_slow_buf[0]);
   bool any_exit_cross = AnyCross(exit_fast_buf[1], exit_slow_buf[1], exit_fast_buf[0], exit_slow_buf[0]);

   if(has_position) {
      if(any_exit_cross) CloseManagedPosition();
      return;
   }

   if(long_entry) {
      OpenManagedPosition(true);
      return;
   }
   if(short_entry) {
      OpenManagedPosition(false);
   }
}
