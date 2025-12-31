//+------------------------------------------------------------------+
//|                                          SwimmyBridge_v14.mq5    |
//|                                  Copyright 2025, Project Swimmy  |
//|                  Ver 14.1 - Multi-Symbol Ticks + Warrior Magic   |
//+------------------------------------------------------------------+
#property copyright "Project Swimmy"
#property version   "14.10"
#include <Trade\Trade.mqh>

// V14.1: Multi-symbol support - send ticks for all these symbols
string g_symbols[] = {"USDJPY", "EURUSD", "GBPUSD"};

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
#define MAGIC_BASE 123456  // Base magic number, actual = MAGIC_BASE + ClanID*10 + WarriorIndex

long g_context = 0;
long g_pub_socket = 0;
long g_sub_socket = 0;
CTrade g_trade;

// V14.0: Track positions by Magic Number (16 warriors possible)
struct WarriorPosition {
   ulong magic;
   ulong ticket;
   string symbol;
   bool active;
};
WarriorPosition g_warriors[16];  // 4 clans x 4 warriors each

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
   Print("🚀 Swimmy Bridge Ver 14.0 - Magic Number per Warrior");
   
   // Initialize warrior tracking
   for(int i = 0; i < 16; i++) {
      g_warriors[i].magic = 0;
      g_warriors[i].ticket = 0;
      g_warriors[i].symbol = "";
      g_warriors[i].active = false;
   }
   
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

// V14.0: Find warrior slot by magic number
int FindWarriorByMagic(ulong magic) {
   for(int i = 0; i < 16; i++) {
      if(g_warriors[i].active && g_warriors[i].magic == magic) {
         return i;
      }
   }
   return -1;
}

// V14.0: Send trade result with magic number info
void SendTradeResult(bool won, double pnl, string symbol, ulong ticket, ulong magic) {
   string json = StringFormat("{\"type\":\"TRADE_CLOSED\",\"won\":%s,\"pnl\":%.2f,\"symbol\":\"%s\",\"ticket\":%I64u,\"magic\":%I64u}",
                              won ? "true" : "false", pnl, symbol, ticket, magic);
   uchar data[];
   StringToCharArray(json, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   Print("📊 Trade Result: ", won ? "WIN" : "LOSS", " PnL: ", pnl, " Symbol: ", symbol, " Magic: ", magic);
}

// V14.0: Check for closed positions by magic number
void CheckPositionsClosed() {
   // Check each tracked warrior
   for(int i = 0; i < 16; i++) {
      if(!g_warriors[i].active) continue;
      
      // Check if position still exists
      bool found = false;
      for(int j = PositionsTotal() - 1; j >= 0; j--) {
         ulong ticket = PositionGetTicket(j);
         if(ticket > 0 && PositionSelectByTicket(ticket)) {
            if(PositionGetInteger(POSITION_MAGIC) == (long)g_warriors[i].magic) {
               found = true;
               break;
            }
         }
      }
      
      // Position closed - find the deal and report
      if(!found) {
         HistorySelect(g_last_history_check, TimeCurrent());
         for(int d = HistoryDealsTotal() - 1; d >= 0; d--) {
            ulong deal = HistoryDealGetTicket(d);
            if(HistoryDealGetInteger(deal, DEAL_MAGIC) == (long)g_warriors[i].magic &&
               HistoryDealGetInteger(deal, DEAL_ENTRY) == DEAL_ENTRY_OUT) {
               double pnl = HistoryDealGetDouble(deal, DEAL_PROFIT);
               string sym = HistoryDealGetString(deal, DEAL_SYMBOL);
               SendTradeResult(pnl > 0, pnl, sym, deal, g_warriors[i].magic);
               break;
            }
         }
         // Mark warrior as inactive
         g_warriors[i].active = false;
         g_warriors[i].magic = 0;
         Print("⚔️ Warrior #", i, " returned from battle");
      }
   }
   g_last_history_check = TimeCurrent();
}

// V14.0: Register new warrior position
int RegisterWarrior(ulong magic, ulong ticket, string symbol) {
   for(int i = 0; i < 16; i++) {
      if(!g_warriors[i].active) {
         g_warriors[i].magic = magic;
         g_warriors[i].ticket = ticket;
         g_warriors[i].symbol = symbol;
         g_warriors[i].active = true;
         Print("⚔️ Warrior #", i, " deployed (Magic: ", magic, ")");
         return i;
      }
   }
   Print("⚠️ All warrior slots full!");
   return -1;
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

// V14.0: Close position by magic number
void CloseByMagic(ulong magic) {
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         if(PositionGetInteger(POSITION_MAGIC) == (long)magic) {
            g_trade.PositionClose(ticket);
            Print("🔒 Closed position with Magic: ", magic);
            return;
         }
      }
   }
}

void CloseAllPositions(string symbol) {
   int closed = 0;
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         string pos_symbol = PositionGetString(POSITION_SYMBOL);
         long pos_magic = PositionGetInteger(POSITION_MAGIC);
         // Only close our positions (magic starts with 123)
         if((symbol == "ALL" || pos_symbol == symbol) && pos_magic >= MAGIC_BASE) {
            if(g_trade.PositionClose(ticket)) {
               closed++;
               Print("🧹 Closed position: ", ticket, " (", pos_symbol, ", Magic: ", pos_magic, ")");
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
   
   // V14.0: Get magic number from command
   ulong cmd_magic = (ulong)GetValueFromJson(cmd, "magic");
   if(cmd_magic <= 0) cmd_magic = MAGIC_BASE;  // Default to base if not specified
   
   if(StringFind(cmd, "\"BUY\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      double vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      
      // Set magic number for this trade
      g_trade.SetExpertMagicNumber((ulong)cmd_magic);
      
      Print("🟢 BUY ", cmd_symbol, " | Vol:", vol, " SL:", sl, " TP:", tp, " Magic:", cmd_magic);
      if(g_trade.Buy(vol, cmd_symbol, 0, sl, tp, "Swimmy Warrior")) {
         RegisterWarrior(cmd_magic, g_trade.ResultOrder(), cmd_symbol);
      }
   }
   else if(StringFind(cmd, "\"SELL\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      double vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      
      // Set magic number for this trade
      g_trade.SetExpertMagicNumber((ulong)cmd_magic);
      
      Print("🔴 SELL ", cmd_symbol, " | Vol:", vol, " SL:", sl, " TP:", tp, " Magic:", cmd_magic);
      if(g_trade.Sell(vol, cmd_symbol, 0, sl, tp, "Swimmy Warrior")) {
         RegisterWarrior(cmd_magic, g_trade.ResultOrder(), cmd_symbol);
      }
   }
   else if(StringFind(cmd, "\"CLOSE\"") >= 0) {
      bool close_all = GetBoolFromJson(cmd, "close_all");
      if(close_all) {
         Print("🧹 CLOSE ALL: ", cmd_symbol);
         CloseAllPositions(cmd_symbol);
      } else if(cmd_magic > MAGIC_BASE) {
         // V14.0: Close by magic number
         Print("⚪ CLOSE by Magic: ", cmd_magic);
         CloseByMagic(cmd_magic);
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
   CheckPositionsClosed();
   
   // V14.1: Send ticks for ALL configured symbols
   for(int s = 0; s < ArraySize(g_symbols); s++) {
      string sym = g_symbols[s];
      double bid = SymbolInfoDouble(sym, SYMBOL_BID);
      double ask = SymbolInfoDouble(sym, SYMBOL_ASK);
      
      // Only send if we got valid prices
      if(bid > 0 && ask > 0) {
         string json_str = StringFormat("{\"type\":\"TICK\",\"symbol\":\"%s\",\"bid\":%.5f,\"ask\":%.5f}",
                                        sym, bid, ask);
         uchar data_snd[];
         StringToCharArray(json_str, data_snd);
         zmq_send(g_pub_socket, data_snd, ArraySize(data_snd)-1, ZMQ_DONTWAIT);
      }
   }
   
   uchar data_rcv[8192];
   int size = zmq_recv(g_sub_socket, data_rcv, 8192, ZMQ_DONTWAIT);
   if(size > 0) {
      ExecuteCommand(CharArrayToString(data_rcv, 0, size));
   }
}
//+------------------------------------------------------------------+

