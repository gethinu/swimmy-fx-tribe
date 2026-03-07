//+------------------------------------------------------------------+
//|                             Legend_RSIReversionV1.mq5            |
//|              Batch-3 port from Swimmy LEGEND inventory           |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "LEGEND batch-3: Legend-RSI-Reversion-V1"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_M5;
input double           InpLots = 0.05;
input ulong            InpMagic = 9307302;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.10;
input double           InpTakeProfitPrice = 0.10;
input int              InpRSIPeriod = 2;
input double           InpOversold = 10.0;
input double           InpOverbought = 90.0;

CTrade g_trade;
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

bool LoadSeries(double &rsi_buf[]) {
   ArraySetAsSeries(rsi_buf, true);
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

   if(is_buy) return g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "Legend-RSI-Reversion-V1");
   return g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "Legend-RSI-Reversion-V1");
}

int OnInit() {
   string symbol = TradeSymbol();
   if(!SymbolSelect(symbol, true)) return INIT_FAILED;

   g_rsi_handle = iRSI(symbol, InpTimeframe, InpRSIPeriod, PRICE_CLOSE);
   if(g_rsi_handle == INVALID_HANDLE) return INIT_FAILED;

   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(g_rsi_handle != INVALID_HANDLE) IndicatorRelease(g_rsi_handle);
}

void OnTick() {
   if(!NewBar()) return;

   double rsi_buf[];
   if(!LoadSeries(rsi_buf)) return;

   double curr_rsi = rsi_buf[0];

   bool long_entry = (curr_rsi < InpOversold);
   bool short_entry = (curr_rsi > InpOverbought);
   bool long_exit = (curr_rsi > InpOverbought);
   bool short_exit = (curr_rsi < InpOversold);

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
