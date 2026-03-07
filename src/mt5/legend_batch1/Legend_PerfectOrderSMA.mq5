//+------------------------------------------------------------------+
//|                                  Legend_PerfectOrderSMA.mq5      |
//|              Batch-1 port from Swimmy LEGEND inventory           |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "LEGEND batch-1: Perfect-Order-SMA"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_M30;
input double           InpLots = 0.01;
input ulong            InpMagic = 9307101;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.30;
input double           InpTakeProfitPrice = 0.90;
input int              InpFastMAPeriod = 20;
input int              InpMidMAPeriod = 50;
input int              InpSlowMAPeriod = 100;

CTrade g_trade;
int g_fast_handle = INVALID_HANDLE;
int g_mid_handle = INVALID_HANDLE;
int g_slow_handle = INVALID_HANDLE;
datetime g_last_bar_time = 0;

string TradeSymbol() {
   return (InpSymbol == "" ? _Symbol : InpSymbol);
}

double NormalizePriceValue(const string symbol, const double price) {
   int digits = (int)SymbolInfoInteger(symbol, SYMBOL_DIGITS);
   return NormalizeDouble(price, digits);
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

bool LoadSeries(double &fast_buf[], double &mid_buf[], double &slow_buf[]) {
   ArraySetAsSeries(fast_buf, true);
   ArraySetAsSeries(mid_buf, true);
   ArraySetAsSeries(slow_buf, true);
   if(CopyBuffer(g_fast_handle, 0, 1, 3, fast_buf) < 3) return false;
   if(CopyBuffer(g_mid_handle, 0, 1, 3, mid_buf) < 3) return false;
   if(CopyBuffer(g_slow_handle, 0, 1, 3, slow_buf) < 3) return false;
   return true;
}

bool CrossBelow(const double prev_left, const double prev_right,
                const double curr_left, const double curr_right) {
   return (prev_left >= prev_right && curr_left < curr_right);
}

bool CrossAbove(const double prev_left, const double prev_right,
                const double curr_left, const double curr_right) {
   return (prev_left <= prev_right && curr_left > curr_right);
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

   if(is_buy) return g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "Perfect-Order-SMA");
   return g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "Perfect-Order-SMA");
}

int OnInit() {
   string symbol = TradeSymbol();
   if(!SymbolSelect(symbol, true)) return INIT_FAILED;

   g_fast_handle = iMA(symbol, InpTimeframe, InpFastMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   g_mid_handle = iMA(symbol, InpTimeframe, InpMidMAPeriod, 0, MODE_SMA, PRICE_CLOSE);
   g_slow_handle = iMA(symbol, InpTimeframe, InpSlowMAPeriod, 0, MODE_SMA, PRICE_CLOSE);

   if(g_fast_handle == INVALID_HANDLE || g_mid_handle == INVALID_HANDLE || g_slow_handle == INVALID_HANDLE)
      return INIT_FAILED;

   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(g_fast_handle != INVALID_HANDLE) IndicatorRelease(g_fast_handle);
   if(g_mid_handle != INVALID_HANDLE) IndicatorRelease(g_mid_handle);
   if(g_slow_handle != INVALID_HANDLE) IndicatorRelease(g_slow_handle);
}

void OnTick() {
   if(!NewBar()) return;

   double fast_buf[];
   double mid_buf[];
   double slow_buf[];
   if(!LoadSeries(fast_buf, mid_buf, slow_buf)) return;

   double curr_fast = fast_buf[0];
   double prev_fast = fast_buf[1];
   double curr_mid = mid_buf[0];
   double prev_mid = mid_buf[1];
   double curr_slow = slow_buf[0];

   bool long_entry = (curr_fast > curr_mid && curr_mid > curr_slow);
   bool short_entry = (curr_fast < curr_mid && curr_mid < curr_slow);
   bool long_exit = CrossBelow(prev_fast, prev_mid, curr_fast, curr_mid);
   bool short_exit = CrossAbove(prev_fast, prev_mid, curr_fast, curr_mid);

   ulong ticket = 0;
   ENUM_POSITION_TYPE type = POSITION_TYPE_BUY;
   bool has_position = GetManagedPosition(ticket, type);

   if(has_position) {
      if(type == POSITION_TYPE_BUY && long_exit) CloseManagedPosition();
      if(type == POSITION_TYPE_SELL && short_exit) CloseManagedPosition();
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
