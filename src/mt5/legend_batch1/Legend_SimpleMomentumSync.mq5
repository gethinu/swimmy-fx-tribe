//+------------------------------------------------------------------+
//|                               Legend_SimpleMomentumSync.mq5      |
//|              Batch-1 port from Swimmy LEGEND inventory           |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "LEGEND batch-1: Simple-Momentum-Sync"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_M30;
input double           InpLots = 0.01;
input ulong            InpMagic = 9307102;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.30;
input double           InpTakeProfitPrice = 0.60;
input int              InpEMAPeriod = 50;
input int              InpRSIPeriod = 14;
input double           InpLongRSIMin = 55.0;
input double           InpShortRSIMax = 45.0;

CTrade g_trade;
int g_ema_handle = INVALID_HANDLE;
int g_rsi_handle = INVALID_HANDLE;
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

bool LoadSeries(double &close_buf[], double &ema_buf[], double &rsi_buf[]) {
   ArraySetAsSeries(close_buf, true);
   ArraySetAsSeries(ema_buf, true);
   ArraySetAsSeries(rsi_buf, true);
   if(CopyClose(TradeSymbol(), InpTimeframe, 1, 3, close_buf) < 3) return false;
   if(CopyBuffer(g_ema_handle, 0, 1, 3, ema_buf) < 3) return false;
   if(CopyBuffer(g_rsi_handle, 0, 1, 3, rsi_buf) < 3) return false;
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

   if(is_buy) return g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "Simple-Momentum-Sync");
   return g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "Simple-Momentum-Sync");
}

int OnInit() {
   string symbol = TradeSymbol();
   if(!SymbolSelect(symbol, true)) return INIT_FAILED;

   g_ema_handle = iMA(symbol, InpTimeframe, InpEMAPeriod, 0, MODE_EMA, PRICE_CLOSE);
   g_rsi_handle = iRSI(symbol, InpTimeframe, InpRSIPeriod, PRICE_CLOSE);
   if(g_ema_handle == INVALID_HANDLE || g_rsi_handle == INVALID_HANDLE) return INIT_FAILED;

   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(g_ema_handle != INVALID_HANDLE) IndicatorRelease(g_ema_handle);
   if(g_rsi_handle != INVALID_HANDLE) IndicatorRelease(g_rsi_handle);
}

void OnTick() {
   if(!NewBar()) return;

   double close_buf[];
   double ema_buf[];
   double rsi_buf[];
   if(!LoadSeries(close_buf, ema_buf, rsi_buf)) return;

   double curr_close = close_buf[0];
   double curr_ema = ema_buf[0];
   double curr_rsi = rsi_buf[0];

   bool long_entry = (curr_close > curr_ema && curr_rsi > InpLongRSIMin);
   bool short_entry = (curr_close < curr_ema && curr_rsi < InpShortRSIMax);
   bool long_exit = (curr_rsi < InpShortRSIMax);
   bool short_exit = (curr_rsi > InpLongRSIMin);

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
