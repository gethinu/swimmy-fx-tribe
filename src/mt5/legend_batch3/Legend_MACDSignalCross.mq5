//+------------------------------------------------------------------+
//|                             Legend_MACDSignalCross.mq5           |
//|              Batch-3 port from Swimmy LEGEND inventory           |
//+------------------------------------------------------------------+
#property strict
#property version   "1.00"
#property description "LEGEND batch-3: MACD-Signal-Cross"

#include <Trade/Trade.mqh>

input string           InpSymbol = "USDJPY";
input ENUM_TIMEFRAMES  InpTimeframe = PERIOD_H4;
input double           InpLots = 0.01;
input ulong            InpMagic = 9307301;
input int              InpSlippagePoints = 30;
input double           InpStopLossPrice = 0.25;
input double           InpTakeProfitPrice = 0.50;
input int              InpFastEMA = 12;
input int              InpSlowEMA = 26;
input int              InpSignalPeriod = 9;

CTrade g_trade;
int g_macd_handle = INVALID_HANDLE;
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

bool LoadSeries(double &main_buf[], double &signal_buf[]) {
   ArraySetAsSeries(main_buf, true);
   ArraySetAsSeries(signal_buf, true);
   if(CopyBuffer(g_macd_handle, 0, 1, 4, main_buf) < 4) return false;
   if(CopyBuffer(g_macd_handle, 1, 1, 4, signal_buf) < 4) return false;
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

   if(is_buy) return g_trade.Buy(InpLots, symbol, 0.0, sl, tp, "MACD-Signal-Cross");
   return g_trade.Sell(InpLots, symbol, 0.0, sl, tp, "MACD-Signal-Cross");
}

int OnInit() {
   string symbol = TradeSymbol();
   if(!SymbolSelect(symbol, true)) return INIT_FAILED;

   g_macd_handle = iMACD(symbol, InpTimeframe, InpFastEMA, InpSlowEMA, InpSignalPeriod, PRICE_CLOSE);
   if(g_macd_handle == INVALID_HANDLE) return INIT_FAILED;

   return INIT_SUCCEEDED;
}

void OnDeinit(const int reason) {
   if(g_macd_handle != INVALID_HANDLE) IndicatorRelease(g_macd_handle);
}

void OnTick() {
   if(!NewBar()) return;

   double main_buf[];
   double signal_buf[];
   if(!LoadSeries(main_buf, signal_buf)) return;

   double curr_main = main_buf[0];
   double prev_main = main_buf[1];
   double curr_signal = signal_buf[0];
   double prev_signal = signal_buf[1];

   bool long_entry = CrossAbove(prev_main, prev_signal, curr_main, curr_signal);
   bool short_entry = CrossBelow(prev_main, prev_signal, curr_main, curr_signal);
   bool long_exit = CrossBelow(prev_main, prev_signal, curr_main, curr_signal);
   bool short_exit = CrossAbove(prev_main, prev_signal, curr_main, curr_signal);

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
