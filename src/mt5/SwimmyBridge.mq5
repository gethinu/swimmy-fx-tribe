//+------------------------------------------------------------------+
//|                                          SwimmyBridge_v15.mq5    |
//|                                  Copyright 2025, Project Swimmy  |
//|                  Ver 15.2 - Multi-Timeframe History Support      |
//+------------------------------------------------------------------+
#property copyright "Project Swimmy"
#property version   "15.20"
#include <Trade\Trade.mqh>

// V15.2: Improvements
// - Connection retry logic
// - Log level control
// - OnTick() for high-frequency tick sending
// - Multi-Timeframe History Support

// ★ User-configurable parameters
input string InpWSL_IP = "172.18.199.122";  // WSL IP Address
input bool   InpVerboseLog = false;         // Verbose Logging (Debug)
input bool   InpUseOnTick = false;          // Use OnTick() for tick sending (high-freq)
input int    InpReconnectInterval = 30;     // Reconnect interval (seconds)

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
#define MAGIC_BASE 123456  // Base magic number

long g_context = 0;
long g_pub_socket = 0;
long g_sub_socket = 0;
CTrade g_trade;

// Connection state for retry logic
bool g_pub_connected = false;
bool g_sub_connected = false;
datetime g_last_reconnect_attempt = 0;

// Track positions by Magic Number (16 warriors possible)
struct WarriorPosition {
   ulong magic;
   ulong ticket;
   string symbol;
   bool active;
};
WarriorPosition g_warriors[16];

datetime g_last_history_check = 0;
datetime g_last_heartbeat = 0;
const int HEARTBEAT_TIMEOUT = 300; // V15.2: Extended from 60s to 300s to avoid false triggers

//+------------------------------------------------------------------+
//| Logging functions with level control                              |
//+------------------------------------------------------------------+
void LogInfo(string msg) {
   Print(msg);
}

void LogDebug(string msg) {
   if(InpVerboseLog) {
      Print("[DEBUG] ", msg);
   }
}

void LogError(string msg) {
   Print("❌ ERROR: ", msg);
}

//+------------------------------------------------------------------+
//| JSON parsing functions                                            |
//+------------------------------------------------------------------+
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

//+------------------------------------------------------------------+
//| Connection functions with retry logic                             |
//+------------------------------------------------------------------+
bool ConnectPubSocket() {
   if(g_pub_socket == 0) {
      g_pub_socket = zmq_socket(g_context, ZMQ_PUB);
   }
   
   string addr_pub_str = "tcp://" + InpWSL_IP + ":5557";
   uchar addr_pub[]; 
   StringToCharArray(addr_pub_str, addr_pub);
   int rc = zmq_connect(g_pub_socket, addr_pub);
   
   if(rc == 0) {
      LogInfo("✅ PUB connected to Guardian: " + addr_pub_str);
      g_pub_connected = true;
      return true;
   } else {
      LogError("Failed to connect PUB socket to " + addr_pub_str);
      g_pub_connected = false;
      return false;
   }
}

bool ConnectSubSocket() {
   if(g_sub_socket == 0) {
      g_sub_socket = zmq_socket(g_context, ZMQ_SUB);
   }
   
   string addr_sub_str = "tcp://" + InpWSL_IP + ":5560";
   uchar addr_sub[]; 
   StringToCharArray(addr_sub_str, addr_sub);
   int rc = zmq_connect(g_sub_socket, addr_sub);
   
   if(rc == 0) {
      uchar filter[]; ArrayResize(filter, 0);
      zmq_setsockopt(g_sub_socket, ZMQ_SUBSCRIBE, filter, 0);
      LogInfo("✅ SUB connected to Guardian: " + addr_sub_str);
      g_sub_connected = true;
      return true;
   } else {
      LogError("Failed to connect SUB socket to " + addr_sub_str);
      g_sub_connected = false;
      return false;
   }
}

void TryReconnect() {
   datetime now = TimeCurrent();
   if((now - g_last_reconnect_attempt) < InpReconnectInterval) {
      return;  // Not time yet
   }
   
   g_last_reconnect_attempt = now;
   
   if(!g_pub_connected) {
      LogInfo("🔄 Attempting to reconnect PUB socket...");
      ConnectPubSocket();
   }
   
   if(!g_sub_connected) {
      LogInfo("🔄 Attempting to reconnect SUB socket...");
      ConnectSubSocket();
   }
}

//+------------------------------------------------------------------+
//| Initialization                                                    |
//+------------------------------------------------------------------+
int OnInit() {
   LogInfo("🚀 Swimmy Bridge Ver 15.2 - Multi-TF Support");
   LogInfo("📊 This EA handles: " + _Symbol + " only");
   LogInfo("📝 Verbose logging: " + (InpVerboseLog ? "ON" : "OFF"));
   LogInfo("⚡ High-freq OnTick: " + (InpUseOnTick ? "ON" : "OFF"));
   
   // Initialize warrior tracking
   for(int i = 0; i < 16; i++) {
      g_warriors[i].magic = 0;
      g_warriors[i].ticket = 0;
      g_warriors[i].symbol = "";
      g_warriors[i].active = false;
   }
   
   g_context = zmq_ctx_new();
   
   // Initial connection attempts
   ConnectPubSocket();
   ConnectSubSocket();
   
   EventSetTimer(1);
   g_last_history_check = TimeCurrent();
   g_last_heartbeat = TimeCurrent();  // Assume alive at start
   g_last_reconnect_attempt = TimeCurrent();
   
   return(INIT_SUCCEEDED);
}

void OnDeinit(const int reason) {
   EventKillTimer();
   if(g_pub_socket != 0) zmq_close(g_pub_socket);
   if(g_sub_socket != 0) zmq_close(g_sub_socket);
   if(g_context != 0) zmq_term(g_context);
   LogInfo("👋 Swimmy Bridge " + _Symbol + " shutdown");
}

//+------------------------------------------------------------------+
//| Warrior management                                                |
//+------------------------------------------------------------------+
int FindWarriorByMagic(ulong magic) {
   for(int i = 0; i < 16; i++) {
      if(g_warriors[i].active && g_warriors[i].magic == magic) {
         return i;
      }
   }
   return -1;
}

void SendTradeResult(bool won, double pnl, string symbol, ulong ticket, ulong magic) {
   if(!g_pub_connected) return;
   
   string json = StringFormat("{\"type\":\"TRADE_CLOSED\",\"won\":%s,\"pnl\":%.2f,\"symbol\":\"%s\",\"ticket\":%I64u,\"magic\":%I64u}",
                              won ? "true" : "false", pnl, symbol, ticket, magic);
   uchar data[];
   StringToCharArray(json, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   LogInfo("📊 Trade Result: " + (won ? "WIN" : "LOSS") + " PnL: " + DoubleToString(pnl, 2) + " Symbol: " + symbol);
}

void CheckPositionsClosed() {
   for(int i = 0; i < 16; i++) {
      if(!g_warriors[i].active) continue;
      
      // Only check positions for THIS symbol
      if(g_warriors[i].symbol != _Symbol) continue;
      
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
         g_warriors[i].active = false;
         g_warriors[i].magic = 0;
         LogDebug("⚔️ Warrior #" + IntegerToString(i) + " returned from battle");
      }
   }
   g_last_history_check = TimeCurrent();
}

int RegisterWarrior(ulong magic, ulong ticket, string symbol) {
   for(int i = 0; i < 16; i++) {
      if(!g_warriors[i].active) {
         g_warriors[i].magic = magic;
         g_warriors[i].ticket = ticket;
         g_warriors[i].symbol = symbol;
         g_warriors[i].active = true;
         LogInfo("⚔️ Warrior #" + IntegerToString(i) + " deployed (Magic: " + IntegerToString(magic) + ")");
         return i;
      }
   }
   LogError("All warrior slots full!");
   return -1;
}

//+------------------------------------------------------------------+
//| History data                                                      |
//+------------------------------------------------------------------+
//+------------------------------------------------------------------+
//| Helper to map string TF to ENUM_TIMEFRAMES                        |
//+------------------------------------------------------------------+
ENUM_TIMEFRAMES StringToTimeframe(string tf) {
   if(tf == "M1") return PERIOD_M1;
   if(tf == "M5") return PERIOD_M5;
   if(tf == "M15") return PERIOD_M15;
   if(tf == "M30") return PERIOD_M30;
   if(tf == "H1") return PERIOD_H1;
   if(tf == "H4") return PERIOD_H4;
   if(tf == "H12") return PERIOD_H12;
   if(tf == "D1") return PERIOD_D1;
   if(tf == "W1") return PERIOD_W1;
   if(tf == "MN") return PERIOD_MN1;
   return PERIOD_M1; // Default
}

//+------------------------------------------------------------------+
//| History data                                                      |
//+------------------------------------------------------------------+
void SendHistoryData(string symbol, string tf, datetime start_time, int count) {
   // Only send history for THIS chart's symbol
   // Note: Empty symbol ("") or "ALL" = broadcast to all EAs
   if(symbol != "" && symbol != "ALL" && symbol != _Symbol) {
      LogDebug("History request for " + symbol + " ignored (this EA handles " + _Symbol + ")");
      return;
   }
   
   if(!g_pub_connected) {
      LogError("Cannot send history - PUB socket not connected");
      return;
   }
   
   ENUM_TIMEFRAMES period = StringToTimeframe(tf);
   
   MqlRates rates[];
   ArraySetAsSeries(rates, true);
   
   // Load more data for higher TFs to ensure deep history, unless count is specified
   if(count <= 0) {
       count = 100000;
       if(period == PERIOD_W1 || period == PERIOD_D1) count = 5000; // 5000 weeks ~ 100 years
   }
   
   int copied = 0;
   // V15.5: Use start_time if provided (Brain sends "start" for gap filling)
   if(start_time > 0) {
       copied = CopyRates(_Symbol, period, start_time, count, rates);
       LogDebug("📊 CopyRates from " + TimeToString(start_time) + " (" + IntegerToString(count) + " bars)");
   } else {
       copied = CopyRates(_Symbol, period, 0, count, rates);
       LogDebug("📊 CopyRates from LATEST (" + IntegerToString(count) + " bars)");
   }
   
   if(copied > 0) {
      LogInfo("📊 Sending " + IntegerToString(copied) + " " + tf + " candles for " + _Symbol + "...");
      
      int batch_size = 5000;
      for(int batch = 0; batch < copied; batch += batch_size) {
         int end = MathMin(batch + batch_size, copied);
         
         // V15.2: Include "tf" in JSON response so Brain knows what it received
         string json = StringFormat("{\"type\":\"HISTORY\",\"symbol\":\"%s\",\"tf\":\"%s\",\"batch\":%d,\"total\":%d,\"data\":[", 
                                    _Symbol, tf, batch / batch_size, (copied / batch_size) + 1);
         
         for(int i = end - 1; i >= batch; i--) {
            json += StringFormat("{\"t\":%I64d,\"o\":%.5f,\"h\":%.5f,\"l\":%.5f,\"c\":%.5f}",
                                 rates[i].time, rates[i].open, rates[i].high, 
                                 rates[i].low, rates[i].close);
            if(i > batch) json += ",";
         }
         json += "]}";
         
         uchar data[];
         StringToCharArray(json, data);
         zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
         
         Sleep(50); // Small delay between batches
      }
      LogInfo("✅ History Sent: " + IntegerToString(copied) + " bars (" + tf + ") for " + _Symbol);
   } else {
      LogError("Failed to copy rates for " + _Symbol + " " + tf + ". Error: " + IntegerToString(GetLastError()));
   }
}

//+------------------------------------------------------------------+
//| Position management                                               |
//+------------------------------------------------------------------+
void CloseByMagic(ulong magic) {
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         if(PositionGetInteger(POSITION_MAGIC) == (long)magic) {
            g_trade.PositionClose(ticket);
            LogInfo("🔒 Closed position with Magic: " + IntegerToString(magic));
            return;
         }
      }
   }
}

void CloseAllPositions(string symbol) {
   // V15: Only close positions for THIS symbol
   if(symbol != "ALL" && symbol != _Symbol) {
      return;  // Not our responsibility
   }
   
   int closed = 0;
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         string pos_symbol = PositionGetString(POSITION_SYMBOL);
         long pos_magic = PositionGetInteger(POSITION_MAGIC);
         
         if((symbol == "ALL" || pos_symbol == _Symbol) && pos_magic >= MAGIC_BASE) {
            if(g_trade.PositionClose(ticket)) {
               closed++;
               LogDebug("🧹 Closed position: " + IntegerToString(ticket) + " (" + pos_symbol + ")");
            }
         }
      }
   }
   if(closed > 0) {
      LogInfo("🧹 Total closed: " + IntegerToString(closed) + " positions for " + _Symbol);
   }
}

//+------------------------------------------------------------------+
//| Command execution                                                 |
//+------------------------------------------------------------------+
void ExecuteCommand(string cmd) {
   string cmd_symbol = GetStringFromJson(cmd, "symbol");
   
   // V15: Only execute commands for THIS symbol
   // Note: Empty symbol ("") or "ALL" = broadcast to all EAs
   if(cmd_symbol != "" && cmd_symbol != "ALL" && cmd_symbol != _Symbol) {
      return;  // Not for this EA
   }
   
   LogDebug("Processing command: " + StringSubstr(cmd, 0, 100) + "...");
   
   ulong cmd_magic = (ulong)GetValueFromJson(cmd, "magic");
   if(cmd_magic <= 0) cmd_magic = MAGIC_BASE;
   
   if(StringFind(cmd, "\"BUY\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      // V15.11: Accept both "lot" and "volume" keys (Brain sends "lot")
      double vol = GetValueFromJson(cmd, "lot");
      if(vol <= 0) vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      
      g_trade.SetExpertMagicNumber((ulong)cmd_magic);
      
      LogInfo("🟢 BUY " + _Symbol + " | Vol:" + DoubleToString(vol, 2) + " SL:" + DoubleToString(sl, 5) + " TP:" + DoubleToString(tp, 5));
      if(g_trade.Buy(vol, _Symbol, 0, sl, tp, "Swimmy Warrior")) {
         RegisterWarrior(cmd_magic, g_trade.ResultOrder(), _Symbol);
      }
   }
   else if(StringFind(cmd, "\"SELL\"") >= 0) {
      double sl = GetValueFromJson(cmd, "sl");
      double tp = GetValueFromJson(cmd, "tp");
      // V15.11: Accept both "lot" and "volume" keys (Brain sends "lot")
      double vol = GetValueFromJson(cmd, "lot");
      if(vol <= 0) vol = GetValueFromJson(cmd, "volume");
      if(vol <= 0) vol = 0.01;
      
      g_trade.SetExpertMagicNumber((ulong)cmd_magic);
      
      LogInfo("🔴 SELL " + _Symbol + " | Vol:" + DoubleToString(vol, 2) + " SL:" + DoubleToString(sl, 5) + " TP:" + DoubleToString(tp, 5));
      if(g_trade.Sell(vol, _Symbol, 0, sl, tp, "Swimmy Warrior")) {
         RegisterWarrior(cmd_magic, g_trade.ResultOrder(), _Symbol);
      }
   }
   else if(StringFind(cmd, "\"CLOSE\"") >= 0) {
      bool close_all = GetBoolFromJson(cmd, "close_all");
      if(close_all) {
         LogInfo("🧹 CLOSE ALL: " + _Symbol);
         CloseAllPositions(_Symbol);
      } else if(cmd_magic > MAGIC_BASE) {
         LogInfo("⚪ CLOSE by Magic: " + IntegerToString(cmd_magic));
         CloseByMagic(cmd_magic);
      } else {
         LogInfo("⚪ CLOSE: " + _Symbol);
         g_trade.PositionClose(_Symbol);
      }
   }
   else if(StringFind(cmd, "\"REQ_HISTORY\"") >= 0) {
      string tf = GetStringFromJson(cmd, "tf");
      if(tf == "") tf = "M1";
      // V15.5: Parse start and count
      datetime start = (datetime)GetValueFromJson(cmd, "start");
      int count = (int)GetValueFromJson(cmd, "count");
      
      SendHistoryData(cmd_symbol, tf, start, count);
   }
   else if(StringFind(cmd, "\"HEARTBEAT\"") >= 0) {
      g_last_heartbeat = TimeCurrent();
      LogDebug("💓 Heartbeat received");
   }
}

//+------------------------------------------------------------------+
//| Tick sending function                                             |
//+------------------------------------------------------------------+
void SendTick() {
   if(!g_pub_connected) return;
   
   double bid = SymbolInfoDouble(_Symbol, SYMBOL_BID);
   double ask = SymbolInfoDouble(_Symbol, SYMBOL_ASK);
   
   if(bid > 0 && ask > 0) {
      string json_str = StringFormat("{\"type\":\"TICK\",\"symbol\":\"%s\",\"bid\":%.5f,\"ask\":%.5f}",
                                     _Symbol, bid, ask);
      uchar data_snd[];
      StringToCharArray(json_str, data_snd);
      zmq_send(g_pub_socket, data_snd, ArraySize(data_snd)-1, ZMQ_DONTWAIT);
   }
}

//+------------------------------------------------------------------+
//| OnTick - High frequency tick sending (optional)                   |
//+------------------------------------------------------------------+
void OnTick() {
   if(InpUseOnTick) {
      SendTick();
   }
}

//+------------------------------------------------------------------+
//| OnTimer - Main processing loop (1 second interval)                |
//+------------------------------------------------------------------+
void OnTimer() {
   // Try reconnect if disconnected
   if(!g_pub_connected || !g_sub_connected) {
      TryReconnect();
   }
   
   CheckPositionsClosed();
   
   // Dead Man's Switch
   if(g_last_heartbeat > 0 && (TimeCurrent() - g_last_heartbeat) > HEARTBEAT_TIMEOUT) {
      // Check if we have positions for THIS symbol
      bool has_positions = false;
      for(int i = PositionsTotal() - 1; i >= 0; i--) {
         ulong ticket = PositionGetTicket(i);
         if(ticket > 0 && PositionSelectByTicket(ticket)) {
            if(PositionGetString(POSITION_SYMBOL) == _Symbol) {
               has_positions = true;
               break;
            }
         }
      }
      
      if(has_positions) {
         LogInfo("💀 DEAD MAN'S SWITCH! Brain silent >60s. EMERGENCY CLOSE " + _Symbol);
         CloseAllPositions(_Symbol);
         g_last_heartbeat = TimeCurrent();
      }
   }

   // Send tick if not using OnTick mode
   if(!InpUseOnTick) {
      SendTick();
   }
   
   // V8.5: Send ACCOUNT_INFO (30秒ごと、Expert Panel P0)
   SendAccountInfo();
   
   // Receive and process commands
   if(g_sub_connected) {
      uchar data_rcv[8192];
      int size = zmq_recv(g_sub_socket, data_rcv, 8192, ZMQ_DONTWAIT);
      if(size > 0) {
         ExecuteCommand(CharArrayToString(data_rcv, 0, size));
      }
   }
}
//+------------------------------------------------------------------+

//+------------------------------------------------------------------+
//| ACCOUNT_INFO送信 (V8.5: Expert Panel P0)                          |
//+------------------------------------------------------------------+
datetime g_last_account_info = 0;
const int ACCOUNT_INFO_INTERVAL = 30; // 30秒ごとに送信

void SendAccountInfo() {
   if(!g_pub_connected) return;
   
   datetime now = TimeCurrent();
   if((now - g_last_account_info) < ACCOUNT_INFO_INTERVAL) return;
   g_last_account_info = now;
   
   // JSON形式でACCOUNT_INFOを構築
   string json = StringFormat(
       "{\"type\":\"ACCOUNT_INFO\",\"timestamp\":%d,\"equity\":%.2f,\"balance\":%.2f,\"margin\":%.2f,\"free_margin\":%.2f,\"margin_level\":%.2f,\"profit\":%.2f,\"leverage\":%d}",
       (int)now,
       AccountInfoDouble(ACCOUNT_EQUITY),
       AccountInfoDouble(ACCOUNT_BALANCE),
       AccountInfoDouble(ACCOUNT_MARGIN),
       AccountInfoDouble(ACCOUNT_MARGIN_FREE),
       AccountInfoDouble(ACCOUNT_MARGIN_LEVEL),
       AccountInfoDouble(ACCOUNT_PROFIT),
       (int)AccountInfoInteger(ACCOUNT_LEVERAGE)
   );
   
   // 既存のPUBソケットで送信
   uchar data[];
   StringToCharArray(json, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   
   LogDebug("[ACCOUNT] Equity=" + DoubleToString(AccountInfoDouble(ACCOUNT_EQUITY), 0));
}