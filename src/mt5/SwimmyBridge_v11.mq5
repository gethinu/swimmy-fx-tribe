//+------------------------------------------------------------------+
//|                                                 SwimmyBridge.mq5 |
//|                                  Copyright 2025, Project Swimmy  |
//|                                     Ver 11.0 - Multi-Position    |
//+------------------------------------------------------------------+
#property copyright "Project Swimmy"
#property version   "11.00"
#include <Trade\Trade.mqh>

#import "libzmq.dll"
   long zmq_ctx_new();
   long zmq_socket(long context, int type);
   int  zmq_bind(long socket, uchar &endpoint[]);
   int  zmq_connect(long socket, uchar &endpoint[]);
   int  zmq_send(long socket, uchar &buf[], int len, int flags);
   int  zmq_recv(long socket, uchar &buf[], int len, int flags);
   int  zmq_setsockopt(long socket, int option, uchar &optval[], int optvallen);
   int  zmq_close(long socket);
   int  zmq_term(long context);
#import

#define ZMQ_PUB 1
#define ZMQ_SUB 2
#define ZMQ_SUBSCRIBE 6
#define ZMQ_DONTWAIT 1
#define MAGIC_NUMBER 123456

long g_context = 0;
long g_pub_socket = 0;
long g_sub_socket = 0;
CTrade g_trade;

// V11.0: Track position COUNT, not just boolean
int g_last_position_count = 0;
datetime g_last_history_check = 0;

double GetValueFromJson(string json, string key) {
   string search = "\"" + key + "\":";
   int start = StringFind(json, search);
   if(start < 0) return 0.0;
   start += StringLen(search);
   int end_comma = StringFind(json, ",", start);
   int end_bracket = StringFind(json, "}", start);
   int end = (end_comma > 0 && end_bracket > 0) ? MathMin(end_comma, end_bracket) : MathMax(end_comma, end_bracket);
   if(end < 0) return 0.0;
   return StringToDouble(StringSubstr(json, start, end - start));
}

string GetStringFromJson(string json, string key) {
   string search = "\"" + key + "\":\"";
   int start = StringFind(json, search);
   if(start < 0) return "";
   start += StringLen(search);
   int end = StringFind(json, "\"", start);
   if(end < 0) return "";
   return StringSubstr(json, start, end - start);
}

bool GetBoolFromJson(string json, string key) {
   string search = "\"" + key + "\":";
   int start = StringFind(json, search);
   if(start < 0) return false;
   start += StringLen(search);
   return (StringFind(json, "true", start) == start);
}

int OnInit() {
   Print("🚀 Swimmy Bridge Ver 11.0 - Multi-Position Fix");
   g_trade.SetExpertMagicNumber(MAGIC_NUMBER);
   g_context = zmq_ctx_new();
   g_pub_socket = zmq_socket(g_context, ZMQ_PUB);
   uchar addr_pub[]; StringToCharArray("tcp://*:5557", addr_pub);
   zmq_bind(g_pub_socket, addr_pub);
   g_sub_socket = zmq_socket(g_context, ZMQ_SUB);
   uchar addr_sub[]; StringToCharArray("tcp://172.18.199.122:5560", addr_sub);
   zmq_connect(g_sub_socket, addr_sub);
   uchar filter[]; ArrayResize(filter, 0);
   zmq_setsockopt(g_sub_socket, ZMQ_SUBSCRIBE, filter, 0);
   EventSetTimer(1);
   g_last_history_check = TimeCurrent();
   return(INIT_SUCCEEDED);
}

void OnDeinit(const int reason) {
   EventKillTimer();
   zmq_close(g_pub_socket);
   zmq_close(g_sub_socket);
   zmq_term(g_context);
}

// === TRADE RESULT NOTIFICATION ===
void SendTradeResult(bool won, double pnl, string symbol, ulong ticket) {
   string json = StringFormat("{\"type\":\"TRADE_CLOSED\",\"won\":%s,\"pnl\":%.2f,\"symbol\":\"%s\",\"ticket\":%I64u}",
                              won ? "true" : "false", pnl, symbol, ticket);
   uchar data[];
   StringToCharArray(json, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   Print("📊 Trade Result Sent: ", won ? "WIN" : "LOSS", " PnL: ", pnl, " Symbol: ", symbol, " Ticket: ", ticket);
}

// === V11.0: POSITION MONITOR - FIXED FOR MULTIPLE POSITIONS ===
void CheckPositionClosed() {
   // Count current positions for this symbol
   int current_count = 0;
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      if(PositionGetSymbol(i) == _Symbol) {
         current_count++;
      }
   }
   
   // Check if any positions were closed
   if(current_count < g_last_position_count) {
      int closed_count = g_last_position_count - current_count;
      Print("📉 Detected ", closed_count, " position(s) closed");
      
      // Check history for ALL recently closed deals
      HistorySelect(g_last_history_check, TimeCurrent());
      int deals = HistoryDealsTotal();
      int sent = 0;
      
      for(int i = deals - 1; i >= 0 && sent < closed_count; i--) {
         ulong ticket = HistoryDealGetTicket(i);
         if(HistoryDealGetInteger(ticket, DEAL_MAGIC) == MAGIC_NUMBER &&
            HistoryDealGetString(ticket, DEAL_SYMBOL) == _Symbol &&
            HistoryDealGetInteger(ticket, DEAL_ENTRY) == DEAL_ENTRY_OUT) {
            double pnl = HistoryDealGetDouble(ticket, DEAL_PROFIT);
            SendTradeResult(pnl > 0, pnl, _Symbol, ticket);
            sent++;
            // V11.0: DON'T break - continue to notify ALL closed positions!
         }
      }
   }
   
   g_last_position_count = current_count;
   g_last_history_check = TimeCurrent();
}

void SendHistoryData(string symbol) {
   MqlRates rates[];
   ArraySetAsSeries(rates, true);
   
   string target_symbol = (symbol == "ALL" || symbol == "") ? _Symbol : symbol;
   int copied = CopyRates(target_symbol, PERIOD_M1, 0, 100000, rates);
   
   if(copied > 0) {
      Print("📊 Sending ", copied, " M1 candles for ", target_symbol, "...");
      
      int batch_size = 5000;
      for(int batch = 0; batch < copied; batch += batch_size) {
         int end = MathMin(batch + batch_size, copied);
         
         string json = StringFormat("{\"type\":\"HISTORY\",\"symbol\":\"%s\",\"batch\":%d,\"total\":%d,\"data\":[", 
                                    target_symbol, batch / batch_size, (copied / batch_size) + 1);
         
         for(int i = end - 1; i >= batch; i--) {
            json += StringFormat("{\"t\":%d,\"o\":%.5f,\"h\":%.5f,\"l\":%.5f,\"c\":%.5f}",
                                 rates[i].time, rates[i].open, rates[i].high, 
                                 rates[i].low, rates[i].close);
            if(i > batch) json += ",";
         }
         json += "]}";
         
         uchar data[];
         StringToCharArray(json, data);
         zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
         
         Sleep(100);
      }
      Print("✅ History Sent: ", copied, " bars (M1) for ", target_symbol);
   }
}

void CloseAllPositions(string symbol) {
   int closed = 0;
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         string pos_symbol = PositionGetString(POSITION_SYMBOL);
         if(symbol == "ALL" || pos_symbol == symbol) {
            if(g_trade.PositionClose(ticket)) {
               closed++;
               Print("🧹 Closed position: ", ticket, " (", pos_symbol, ")");
            }
         }
      }
   }
   if(closed > 0) {
      Print("🧹 Total closed: ", closed, " positions");
   }
}

void ExecuteCommand(string cmd) {
   string cmd_symbol = GetStringFromJson(cmd, "symbol");
   if(cmd_symbol == "") cmd_symbol = _Symbol;
   
   if(StringFind(cmd, "\"BUY\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      double vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      Print("🟢 BUY ", cmd_symbol, " | Vol:", vol, " SL:", sl, " TP:", tp);
      g_trade.Buy(vol, cmd_symbol, 0, sl, tp, "Swimmy AI");
   }
   else if(StringFind(cmd, "\"SELL\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      double vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      Print("🔴 SELL ", cmd_symbol, " | Vol:", vol, " SL:", sl, " TP:", tp);
      g_trade.Sell(vol, cmd_symbol, 0, sl, tp, "Swimmy AI");
   }
   else if(StringFind(cmd, "\"CLOSE\"") >= 0) {
      bool close_all = GetBoolFromJson(cmd, "close_all");
      if(close_all) {
         Print("🧹 CLOSE ALL: ", cmd_symbol);
         CloseAllPositions(cmd_symbol);
      } else {
         Print("⚪ CLOSE: ", cmd_symbol);
         g_trade.PositionClose(cmd_symbol);
      }
   }
   else if(StringFind(cmd, "\"REQ_HISTORY\"") >= 0) {
      SendHistoryData(cmd_symbol);
   }
}

void OnTimer() {
   CheckPositionClosed();
   
   string json_str = StringFormat("{\"type\":\"TICK\",\"symbol\":\"%s\",\"bid\":%.5f,\"ask\":%.5f}",
                                  _Symbol, SymbolInfoDouble(_Symbol, SYMBOL_BID), SymbolInfoDouble(_Symbol, SYMBOL_ASK));
   uchar data_snd[];
   StringToCharArray(json_str, data_snd);
   zmq_send(g_pub_socket, data_snd, ArraySize(data_snd)-1, ZMQ_DONTWAIT);
   
   uchar data_rcv[8192];
   int size = zmq_recv(g_sub_socket, data_rcv, 8192, ZMQ_DONTWAIT);
   if(size > 0) {
      ExecuteCommand(CharArrayToString(data_rcv, 0, size));
   }
}
//+------------------------------------------------------------------+
