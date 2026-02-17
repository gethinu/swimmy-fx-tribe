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
input string InpWSL_IP = "";  // WSL IP Address
input int    InpPortMarket = 5557;          // ZMQ Market Data (MT5 -> Guardian)
input int    InpPortExec = 5560;            // ZMQ Execution (Guardian -> MT5)
input bool   InpVerboseLog = false;         // Verbose Logging (Debug)
input bool   InpUseOnTick = false;          // Use OnTick() for tick sending (high-freq)
input int    InpReconnectInterval = 30;     // Reconnect interval (seconds)
input bool   InpLogSymbolMismatch = true;   // Log ignored symbol-mismatch commands (throttled)

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
   double entry_price;
   bool active;
};
WarriorPosition g_warriors[16];

datetime g_last_history_check = 0;
datetime g_last_heartbeat = 0;
const int HEARTBEAT_TIMEOUT = 300; // V15.2: Extended from 60s to 300s to avoid false triggers
const int MAX_COMMANDS_PER_TIMER = 64; // Drain execution queue in bursts to avoid command backlog.
const int ORDER_REPLY_MAX_ATTEMPTS = 3; // ACK/REJECT delivery is at-least-once with short retries.
const int ORDER_REPLY_RETRY_DELAY_MS = 20;
datetime g_last_symbol_mismatch_log = 0;
int g_symbol_mismatch_count = 0;

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
//| S-expression parsing helpers                                      |
//+------------------------------------------------------------------+
string EscapeSexpString(string value) {
   string out = value;
   StringReplace(out, "\\", "\\\\");
   StringReplace(out, "\"", "\\\"");
   return out;
}

string ToLowerCopy(string value) {
   string out = value;
   StringToLower(out);
   return out;
}

string ToUpperCopy(string value) {
   string out = value;
   StringToUpper(out);
   return out;
}

void BuildQuotedStringMask(string text, bool &inside[]) {
   int n = StringLen(text);
   ArrayResize(inside, n);

   bool in_string = false;
   bool escaped = false;
   for(int i = 0; i < n; i++) {
      inside[i] = in_string;

      int ch = StringGetCharacter(text, i);
      if(in_string) {
         if(!escaped && ch == 92) { // \
            escaped = true;
            continue;
         }
         if(!escaped && ch == 34) { // "
            in_string = false;
         }
         escaped = false;
      } else {
         if(ch == 34) { // "
            in_string = true;
            escaped = false;
         }
      }
   }
}

bool IsIndexInsideQuotedMask(bool &inside[], int index) {
   if(index < 0) return false;
   if(index >= ArraySize(inside)) return false;
   return inside[index];
}

int FindPatternOutsideQuotedStrings(string text, string pattern, bool &inside[]) {
   if(pattern == "") return -1;
   int search_from = 0;
   while(true) {
      int hit = StringFind(text, pattern, search_from);
      if(hit < 0) return -1;
      if(!IsIndexInsideQuotedMask(inside, hit)) {
         return hit;
      }
      search_from = hit + 1;
   }
}

bool FindSexpValue(string sexp, string key, string &value, bool &is_string) {
   string normalized = ToLowerCopy(sexp);
   string key_lc = ToLowerCopy(key);
   string patterns[];
   ArrayResize(patterns, 24);
   patterns[0] = "(" + key_lc + " . ";
   patterns[1] = "(:" + key_lc + " . ";
   patterns[2] = "(swimmy.core::" + key_lc + " . ";
   patterns[3] = "(swimmy.school::" + key_lc + " . ";
   patterns[4] = "(swimmy.executor::" + key_lc + " . ";
   patterns[5] = "(swimmy.main::" + key_lc + " . ";
   patterns[6] = "(swimmy.globals::" + key_lc + " . ";
   patterns[7] = "(swimmy.core:" + key_lc + " . ";
   patterns[8] = "(swimmy.school:" + key_lc + " . ";
   patterns[9] = "(swimmy.executor:" + key_lc + " . ";
   patterns[10] = "(swimmy.main:" + key_lc + " . ";
   patterns[11] = "(swimmy.globals:" + key_lc + " . ";
   // Shorthand key-value forms: (key value)
   patterns[12] = "(" + key_lc + " ";
   patterns[13] = "(:" + key_lc + " ";
   patterns[14] = "(swimmy.core::" + key_lc + " ";
   patterns[15] = "(swimmy.school::" + key_lc + " ";
   patterns[16] = "(swimmy.executor::" + key_lc + " ";
   patterns[17] = "(swimmy.main::" + key_lc + " ";
   patterns[18] = "(swimmy.globals::" + key_lc + " ";
   patterns[19] = "(swimmy.core:" + key_lc + " ";
   patterns[20] = "(swimmy.school:" + key_lc + " ";
   patterns[21] = "(swimmy.executor:" + key_lc + " ";
   patterns[22] = "(swimmy.main:" + key_lc + " ";
   patterns[23] = "(swimmy.globals:" + key_lc + " ";

   bool inside_quotes[];
   BuildQuotedStringMask(normalized, inside_quotes);

   int start = -1;
   int matched_len = 0;
   for(int i = 0; i < ArraySize(patterns); i++) {
      int hit = FindPatternOutsideQuotedStrings(normalized, patterns[i], inside_quotes);
      if(hit < 0) continue;

      int hit_len = StringLen(patterns[i]);
      if(start < 0 || hit < start || (hit == start && hit_len > matched_len)) {
         start = hit;
         matched_len = hit_len;
      }
   }
   if(start < 0) return false;
   start += matched_len;

   while(start < StringLen(sexp) && StringGetCharacter(sexp, start) == ' ') start++;
   if(start >= StringLen(sexp)) return false;

   if(StringGetCharacter(sexp, start) == 34) { // "
      is_string = true;
      int value_start = start + 1;
      int end = value_start;
      bool escaped = false;
      while(end < StringLen(sexp)) {
         int ch = StringGetCharacter(sexp, end);
         if(!escaped && ch == 92) { // \
            escaped = true;
            end++;
            continue;
         }
         if(!escaped && ch == 34) { // "
            break;
         }
         escaped = false;
         end++;
      }
      if(end >= StringLen(sexp)) return false;
      value = StringSubstr(sexp, value_start, end - value_start);
      return true;
   }

   if(StringGetCharacter(sexp, start) == 40) { // (
      // List value, e.g. (key (a b)).
      is_string = false;
      int end = start;
      int depth = 0;
      while(end < StringLen(sexp)) {
         int ch = StringGetCharacter(sexp, end);
         if(ch == 40) depth++;
         if(ch == 41) {
            depth--;
            if(depth == 0) {
               end++;
               break;
            }
         }
         end++;
      }
      if(depth != 0 || end <= start) return false;
      value = StringSubstr(sexp, start, end - start);
      return true;
   }

   is_string = false;
   int end_token = start;
   while(end_token < StringLen(sexp)) {
      int ch = StringGetCharacter(sexp, end_token);
      if(ch == ' ' || ch == ')') break;
      end_token++;
   }
   if(end_token <= start) return false;
   value = StringSubstr(sexp, start, end_token - start);
   StringTrimLeft(value);
   StringTrimRight(value);
   return true;
}

double GetValueFromSexp(string sexp, string key) {
   string raw;
   bool is_string = false;
   if(!FindSexpValue(sexp, key, raw, is_string)) return 0.0;
   return StringToDouble(raw);
}

string GetStringFromSexp(string sexp, string key) {
   string raw;
   bool is_string = false;
   if(!FindSexpValue(sexp, key, raw, is_string)) return "";
   return raw;
}

bool GetBoolFromSexp(string sexp, string key) {
   string raw;
   bool is_string = false;
   if(!FindSexpValue(sexp, key, raw, is_string)) return false;
   // MQL5 StringToLower mutates the string (returns bool), so don't assign its return value.
   string lowered = raw;
   StringToLower(lowered);
   if(lowered == "true" || lowered == "t" || lowered == "1" || lowered == "#t") return true;
   if(lowered == "false" || lowered == "nil" || lowered == "0" || lowered == "#f") return false;
   return false;
}

string NormalizeToken(string token) {
   string out = token;
   StringTrimLeft(out);
   StringTrimRight(out);
   StringToUpper(out);
   return out;
}

bool IsAllDigits(string value) {
   int n = StringLen(value);
   if(n <= 0) return false;
   for(int i = 0; i < n; i++) {
      int ch = StringGetCharacter(value, i);
      if(ch < '0' || ch > '9') return false;
   }
   return true;
}

bool TryParseTimeframeTokenToMinutes(string tf_raw, int &minutes) {
   string tf = NormalizeToken(tf_raw);
   minutes = 0;
   if(tf == "") return false;

   if(IsAllDigits(tf)) {
      minutes = (int)StringToInteger(tf);
      return (minutes > 0);
   }

   if(tf == "MN" || tf == "MN1") {
      minutes = 43200;
      return true;
   }

   if(StringLen(tf) < 2) return false;
   int prefix = StringGetCharacter(tf, 0);
   string suffix = StringSubstr(tf, 1);
   if(!IsAllDigits(suffix)) return false;
   int unit = (int)StringToInteger(suffix);
   if(unit <= 0) return false;

   if(prefix == 'M') {
      minutes = unit;
      return true;
   }
   if(prefix == 'H') {
      minutes = unit * 60;
      return true;
   }
   if(prefix == 'D') {
      minutes = unit * 1440;
      return true;
   }
   if(prefix == 'W') {
      minutes = unit * 10080;
      return true;
   }
   return false;
}

bool TryMinutesToStandardTimeframe(int minutes, ENUM_TIMEFRAMES &period, string &tf_label) {
   switch(minutes) {
      case 1:     period = PERIOD_M1;   tf_label = "M1";  return true;
      case 5:     period = PERIOD_M5;   tf_label = "M5";  return true;
      case 15:    period = PERIOD_M15;  tf_label = "M15"; return true;
      case 30:    period = PERIOD_M30;  tf_label = "M30"; return true;
      case 60:    period = PERIOD_H1;   tf_label = "H1";  return true;
      case 240:   period = PERIOD_H4;   tf_label = "H4";  return true;
      case 720:   period = PERIOD_H12;  tf_label = "H12"; return true;
      case 1440:  period = PERIOD_D1;   tf_label = "D1";  return true;
      case 10080: period = PERIOD_W1;   tf_label = "W1";  return true;
      case 43200: period = PERIOD_MN1;  tf_label = "MN1"; return true;
   }
   return false;
}

bool IsNilLikeToken(string token) {
   string normalized = NormalizeToken(token);
   return (normalized == "" || normalized == "NIL" || normalized == "NULL" || normalized == "NONE");
}

bool ValidateOrderComment(string comment, string &reason) {
   string trimmed = comment;
   StringTrimLeft(trimmed);
   StringTrimRight(trimmed);
   if(trimmed == "") {
      reason = "MISSING_COMMENT";
      return false;
   }

   int delim = StringFind(trimmed, "|");
   if(delim <= 0 || delim >= StringLen(trimmed) - 1) {
      reason = "INVALID_COMMENT_FORMAT";
      return false;
   }
   if(StringFind(trimmed, "|", delim + 1) >= 0) {
      reason = "INVALID_COMMENT_FORMAT";
      return false;
   }

   string strategy = StringSubstr(trimmed, 0, delim);
   string tf_token = StringSubstr(trimmed, delim + 1);
   StringTrimLeft(strategy);
   StringTrimRight(strategy);
   StringTrimLeft(tf_token);
   StringTrimRight(tf_token);

   if(IsNilLikeToken(strategy)) {
      reason = "MISSING_STRATEGY";
      return false;
   }
   if(IsNilLikeToken(tf_token)) {
      reason = "MISSING_TIMEFRAME";
      return false;
   }

   int tf_minutes = 0;
   if(!TryParseTimeframeTokenToMinutes(tf_token, tf_minutes) || tf_minutes <= 0) {
      reason = "INVALID_COMMENT_TF";
      return false;
   }

   reason = "";
   return true;
}

//+------------------------------------------------------------------+
//| Connection functions with retry logic                             |
//+------------------------------------------------------------------+
bool ConnectPubSocket() {
   if(g_context == 0) {
      LogError("Cannot connect PUB socket: ZMQ context is null");
      g_pub_connected = false;
      return false;
   }

   if(g_pub_socket != 0 && !g_pub_connected) {
      zmq_close(g_pub_socket);
      g_pub_socket = 0;
   }

   if(g_pub_socket == 0) {
      g_pub_socket = zmq_socket(g_context, ZMQ_PUB);
      if(g_pub_socket == 0) {
         LogError("Failed to create PUB socket");
         g_pub_connected = false;
         return false;
      }
   }
   
   string addr_pub_str = "tcp://" + InpWSL_IP + ":" + IntegerToString(InpPortMarket);
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
   if(g_context == 0) {
      LogError("Cannot connect SUB socket: ZMQ context is null");
      g_sub_connected = false;
      return false;
   }

   if(g_sub_socket != 0 && !g_sub_connected) {
      zmq_close(g_sub_socket);
      g_sub_socket = 0;
   }

   if(g_sub_socket == 0) {
      g_sub_socket = zmq_socket(g_context, ZMQ_SUB);
      if(g_sub_socket == 0) {
         LogError("Failed to create SUB socket");
         g_sub_connected = false;
         return false;
      }
   }
   
   string addr_sub_str = "tcp://" + InpWSL_IP + ":" + IntegerToString(InpPortExec);
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

   if(InpWSL_IP == "") {
      LogError("InpWSL_IP is empty. Set the WSL IP in EA inputs.");
      return(INIT_FAILED);
   }
   
   // Initialize warrior tracking
   for(int i = 0; i < 16; i++) {
      g_warriors[i].magic = 0;
      g_warriors[i].ticket = 0;
      g_warriors[i].symbol = "";
      g_warriors[i].entry_price = 0.0;
      g_warriors[i].active = false;
   }
   
   g_context = zmq_ctx_new();
   if(g_context == 0) {
      LogError("Failed to create ZMQ context");
      return(INIT_FAILED);
   }
   
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
   g_pub_socket = 0;
   g_sub_socket = 0;
   g_context = 0;
   g_pub_connected = false;
   g_sub_connected = false;
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

void SendTradeResult(bool won, double pnl, string symbol, ulong ticket, ulong magic, double entry_price, double exit_price) {
   if(!g_pub_connected) return;
   
   string sexp = StringFormat("((type . \"TRADE_CLOSED\") (won . %s) (pnl . %.2f) (symbol . \"%s\") (ticket . %I64u) (magic . %I64u) (entry_price . %.5f) (exit_price . %.5f))",
                              won ? "true" : "false", pnl, EscapeSexpString(symbol), ticket, magic, entry_price, exit_price);
   uchar data[];
   StringToCharArray(sexp, data);
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
               double exit_price = HistoryDealGetDouble(deal, DEAL_PRICE);
               double entry_price = g_warriors[i].entry_price;
               SendTradeResult(pnl > 0, pnl, sym, deal, g_warriors[i].magic, entry_price, exit_price);
               break;
            }
         }
         g_warriors[i].active = false;
         g_warriors[i].magic = 0;
         g_warriors[i].entry_price = 0.0;
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
         g_warriors[i].entry_price = 0.0;
         g_warriors[i].active = true;
         if(PositionSelectByTicket(ticket)) {
            g_warriors[i].entry_price = PositionGetDouble(POSITION_PRICE_OPEN);
         }
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
//| NOTE: MT5 supports a fixed set of ENUM_TIMEFRAMES. Custom TFs     |
//| should be resampled from M1 on the Brain/DataKeeper side.         |
//+------------------------------------------------------------------+
bool TryStringToTimeframe(string tf, ENUM_TIMEFRAMES &period) {
   int minutes = 0;
   string tf_label = "";
   if(!TryParseTimeframeTokenToMinutes(tf, minutes)) {
      return false;
   }
   return TryMinutesToStandardTimeframe(minutes, period, tf_label);
}

//+------------------------------------------------------------------+
//| History data                                                      |
//+------------------------------------------------------------------+
void SendHistoryData(string symbol, string tf, datetime start_time, int count) {
   // Only send history for THIS chart's symbol
   // Note: Empty symbol ("") or "ALL" = broadcast to all EAs
   string symbol_norm = ToUpperCopy(symbol);
   string this_symbol = ToUpperCopy(_Symbol);
   if(symbol_norm != "" && symbol_norm != "ALL" && symbol_norm != this_symbol) {
      LogDebug("History request for " + symbol + " ignored (this EA handles " + _Symbol + ")");
      return;
   }
   
   if(!g_pub_connected) {
      LogError("Cannot send history - PUB socket not connected");
      return;
   }
   
   ENUM_TIMEFRAMES period = PERIOD_M1;
   string tf_label = "M1";
   int requested_minutes = 0;
   bool has_minutes = TryParseTimeframeTokenToMinutes(tf, requested_minutes);
   if(has_minutes) {
      if(!TryMinutesToStandardTimeframe(requested_minutes, period, tf_label)) {
         // Non-standard minutes must be resampled from M1 in upper layers.
         LogInfo("⚠️ Custom TF '" + tf + "' (" + IntegerToString(requested_minutes) + "m) requested; sending M1 for resampling.");
         period = PERIOD_M1;
         tf_label = "M1";
      }
   } else if(!TryStringToTimeframe(tf, period)) {
      LogInfo("⚠️ Unsupported TF '" + tf + "' requested; sending M1 for resampling.");
      period = PERIOD_M1;
      tf_label = "M1";
   }
   
   MqlRates rates[];
   ArraySetAsSeries(rates, true);
   
   // Load deeper history for higher TFs to avoid sparse-trade validation windows.
   if(count <= 0) {
       count = 100000;
       if(period == PERIOD_D1)  count = 20000; // ~54 years
       if(period == PERIOD_W1)  count = 10000; // ~192 years (bounded by broker history)
       if(period == PERIOD_MN1) count = 2400;  // ~200 years (bounded by broker history)
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
      LogInfo("📊 Sending " + IntegerToString(copied) + " " + tf_label + " candles for " + _Symbol + "...");
      
      int batch_size = 5000;
      int total_batches = (copied + batch_size - 1) / batch_size;
      for(int batch = 0; batch < copied; batch += batch_size) {
         int end = MathMin(batch + batch_size, copied);
         
         string sexp = StringFormat("((type . \"HISTORY\") (symbol . \"%s\") (tf . \"%s\") (batch . %d) (total . %d) (data . (", 
                                    EscapeSexpString(_Symbol), EscapeSexpString(tf_label), batch / batch_size, total_batches);
         
         for(int i = end - 1; i >= batch; i--) {
            sexp += StringFormat("((t . %I64d) (o . %.5f) (h . %.5f) (l . %.5f) (c . %.5f))",
                                 rates[i].time, rates[i].open, rates[i].high,
                                 rates[i].low, rates[i].close);
            if(i > batch) sexp += " ";
         }
         sexp += "))";
         
         uchar data[];
         StringToCharArray(sexp, data);
         zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
         
         Sleep(50); // Small delay between batches
      }
      LogInfo("✅ History Sent: " + IntegerToString(copied) + " bars (" + tf_label + ") for " + _Symbol);
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
//| V17: Close only short timeframe positions (H4 and below)          |
//| Protects D1, W1, MN positions during emergency close              |
//+------------------------------------------------------------------+
void CloseShortTimeframePositions(string symbol) {
   if(symbol != "ALL" && symbol != _Symbol) {
      return;
   }
   
   int closed = 0;
   int protected_tf = 0;
   
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         string pos_symbol = PositionGetString(POSITION_SYMBOL);
         long pos_magic = PositionGetInteger(POSITION_MAGIC);
         string comment = PositionGetString(POSITION_COMMENT);
         
         if((symbol == "ALL" || pos_symbol == _Symbol) && pos_magic >= MAGIC_BASE) {
            // Protect D1+ positions by timeframe token in comment (strategy|tf).
            bool is_protected = false;
            int tf_sep = StringFind(comment, "|");
            if(tf_sep > 0 && tf_sep < StringLen(comment) - 1) {
               string tf_token = StringSubstr(comment, tf_sep + 1);
               int tf_minutes = 0;
               if(TryParseTimeframeTokenToMinutes(tf_token, tf_minutes) && tf_minutes >= 1440) {
                  is_protected = true;
               }
            }
            if(!is_protected) {
               // Backward-compatible fallback for legacy comment formats.
               is_protected = (StringFind(comment, "|D1") >= 0 ||
                               StringFind(comment, "|W1") >= 0 ||
                               StringFind(comment, "|MN") >= 0 ||
                               StringFind(comment, "|MN1") >= 0);
            }
            
            if(is_protected) {
               protected_tf++;
               LogInfo("🛡️ PROTECTED (D1+): Ticket " + IntegerToString(ticket) + " [" + comment + "]");
            } else {
               if(g_trade.PositionClose(ticket)) {
                  closed++;
                  LogDebug("🧹 Closed (short TF): " + IntegerToString(ticket) + " [" + comment + "]");
               }
            }
         }
      }
   }
   LogInfo("🧹 CLOSE_SHORT_TF: Closed " + IntegerToString(closed) + ", Protected D1+: " + IntegerToString(protected_tf));
}

//+------------------------------------------------------------------+
//| Command execution                                                 |
//+------------------------------------------------------------------+
void ExecuteCommand(string cmd) {
   string type = GetStringFromSexp(cmd, "type");
   type = ToUpperCopy(type);

   // HEARTBEAT must be handled before symbol routing.
   if(type == "HEARTBEAT") {
      g_last_heartbeat = TimeCurrent();
      LogDebug("💓 Heartbeat received");
      return;
   }

   string instrument = GetStringFromSexp(cmd, "instrument");
   string cmd_symbol = ToUpperCopy(instrument);
   if(cmd_symbol == "") cmd_symbol = ToUpperCopy(GetStringFromSexp(cmd, "symbol"));
   if(IsNilLikeToken(cmd_symbol)) cmd_symbol = "";
   string this_symbol = ToUpperCopy(_Symbol);

   // Only execute commands for THIS symbol (or ALL)
   if(cmd_symbol != "" && cmd_symbol != "ALL" && cmd_symbol != this_symbol) {
      g_symbol_mismatch_count++;
      if(InpLogSymbolMismatch && (TimeCurrent() - g_last_symbol_mismatch_log >= 30)) {
         LogInfo("⚠️ Ignored command for symbol " + cmd_symbol +
                 " on chart " + _Symbol +
                 " (mismatch count=" + IntegerToString(g_symbol_mismatch_count) + ")");
         g_last_symbol_mismatch_log = TimeCurrent();
      }
      return;  // Not for this EA
   }

   LogDebug("Processing command: " + StringSubstr(cmd, 0, 100) + "...");
   
   ulong cmd_magic = (ulong)GetValueFromSexp(cmd, "magic");
   if(cmd_magic <= 0) cmd_magic = MAGIC_BASE;

   // ORDER_OPEN (Protocol V2)
   if(type == "ORDER_OPEN") {
      if(cmd_symbol == "ALL") {
         LogError("ORDER_OPEN does not support symbol=ALL");
         SendTradeReject(cmd, "INVALID_INSTRUMENT", 0);
         return;
      }
      if(cmd_symbol == "") {
         LogError("ORDER_OPEN missing instrument/symbol");
         SendTradeReject(cmd, "MISSING_INSTRUMENT", 0);
         return;
      }
      string side = ToUpperCopy(GetStringFromSexp(cmd, "side"));
      if(side != "BUY" && side != "SELL") {
         LogError("ORDER_OPEN invalid side: " + side);
         SendTradeReject(cmd, "INVALID_SIDE", 0);
         return;
      }
      double sl = GetValueFromSexp(cmd, "sl");
      double tp = GetValueFromSexp(cmd, "tp");
      double vol = GetValueFromSexp(cmd, "lot");
      if(vol <= 0) vol = 0.01;

      string comment = GetStringFromSexp(cmd, "comment");
      string comment_reason = "";
      if(!ValidateOrderComment(comment, comment_reason)) {
         LogError("ORDER_OPEN invalid comment: " + comment + " reason=" + comment_reason);
         SendTradeReject(cmd, comment_reason, 0);
         return;
      }
      StringTrimLeft(comment);
      StringTrimRight(comment);

      g_trade.SetExpertMagicNumber((ulong)cmd_magic);

      if(side == "BUY") {
         LogInfo("🟢 BUY " + _Symbol + " | Vol:" + DoubleToString(vol, 2) + " SL:" + DoubleToString(sl, 5) + " TP:" + DoubleToString(tp, 5) + " [" + comment + "]");
         if(g_trade.Buy(vol, _Symbol, 0, sl, tp, comment)) {
            RegisterWarrior(cmd_magic, g_trade.ResultOrder(), _Symbol);
            SendTradeAck(cmd, g_trade.ResultOrder());
         } else {
            long rc = g_trade.ResultRetcode();
            string reason = g_trade.ResultRetcodeDescription();
            if(reason == "") reason = "BUY_FAILED";
            LogError("ORDER_OPEN BUY failed: " + reason + " (retcode=" + IntegerToString((int)rc) + ")");
            SendTradeReject(cmd, reason, rc);
         }
      } else if(side == "SELL") {
         LogInfo("🔴 SELL " + _Symbol + " | Vol:" + DoubleToString(vol, 2) + " SL:" + DoubleToString(sl, 5) + " TP:" + DoubleToString(tp, 5) + " [" + comment + "]");
         if(g_trade.Sell(vol, _Symbol, 0, sl, tp, comment)) {
            RegisterWarrior(cmd_magic, g_trade.ResultOrder(), _Symbol);
            SendTradeAck(cmd, g_trade.ResultOrder());
         } else {
            long rc = g_trade.ResultRetcode();
            string reason = g_trade.ResultRetcodeDescription();
            if(reason == "") reason = "SELL_FAILED";
            LogError("ORDER_OPEN SELL failed: " + reason + " (retcode=" + IntegerToString((int)rc) + ")");
            SendTradeReject(cmd, reason, rc);
         }
      }
   }
   else if(type == "CLOSE") {
      bool close_all = GetBoolFromSexp(cmd, "close_all");
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
   else if(type == "CLOSE_SHORT_TF") {
      LogInfo("🧹 CLOSE_SHORT_TF: Closing H4 and below (D1+ protected)");
      CloseShortTimeframePositions(_Symbol);
   }
   else if(type == "REQ_HISTORY") {
      string tf = GetStringFromSexp(cmd, "tf");
      if(tf == "") tf = "M1";
      datetime start = (datetime)GetValueFromSexp(cmd, "start");
      int count = (int)GetValueFromSexp(cmd, "count");
      SendHistoryData(cmd_symbol, tf, start, count);
   }
   else if(type == "GET_POSITIONS") {
      SendOpenPositions();
   }
   else if(type == "GET_SWAP") {
      SendSwapData();
   } else {
      LogError("Unknown command type [" + type + "] raw=" + StringSubstr(cmd, 0, 140));
   }
}

//+------------------------------------------------------------------+
//| V30: Send Swap Data (Phase 28/29)                                 |
//+------------------------------------------------------------------+
void SendSwapData() {
   if(!g_pub_connected) return;
   
   double swap_long = SymbolInfoDouble(_Symbol, SYMBOL_SWAP_LONG);
   double swap_short = SymbolInfoDouble(_Symbol, SYMBOL_SWAP_SHORT);
   
   // Check swap mode (0=disabled, 1=points, 2=currency, 3=interests, 4=currency_margin)
   // We will normalize to "points" roughly or just send raw and let Python handle it.
   // Let's send raw values + mode.
   long swap_mode = SymbolInfoInteger(_Symbol, SYMBOL_SWAP_MODE);
   
   // Get Spread as well
   int spread = (int)SymbolInfoInteger(_Symbol, SYMBOL_SPREAD);
   
   string sexp = StringFormat("((type . \"SWAP_DATA\") (symbol . \"%s\") (swap_long . %.5f) (swap_short . %.5f) (swap_mode . %d) (spread . %d))",
                              EscapeSexpString(_Symbol), swap_long, swap_short, swap_mode, spread);
   
   uchar data[];
   StringToCharArray(sexp, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   LogInfo("📊 SWAP DATA sent: L=" + DoubleToString(swap_long, 2) + " S=" + DoubleToString(swap_short, 2));
}

bool SendPubWithRetry(string sexp, int max_attempts, int retry_delay_ms, int &attempts_used) {
   attempts_used = 0;
   if(!g_pub_connected) return false;

   int attempts = max_attempts;
   if(attempts < 1) attempts = 1;

   uchar data[];
   StringToCharArray(sexp, data);
   int payload_len = ArraySize(data) - 1;
   if(payload_len <= 0) return false;

   int last_rc = -1;
   for(int attempt = 1; attempt <= attempts; attempt++) {
      attempts_used = attempt;
      int rc = zmq_send(g_pub_socket, data, payload_len, ZMQ_DONTWAIT);
      last_rc = rc;
      if(rc >= 0) {
         return true;
      }
      if(attempt < attempts && retry_delay_ms > 0) {
         Sleep(retry_delay_ms);
      }
   }
   if(last_rc < 0) {
      g_pub_connected = false;
   }
   return false;
}

// Helper to send ACK
void SendTradeAck(string orig_cmd, ulong ticket) {
   if(!g_pub_connected) return;
   string id = GetStringFromSexp(orig_cmd, "id");
   string sexp = StringFormat("((type . \"ORDER_ACK\") (id . \"%s\") (ticket . %I64u) (symbol . \"%s\"))", 
                              EscapeSexpString(id), ticket, EscapeSexpString(_Symbol));
   int attempts_used = 0;
   bool sent = SendPubWithRetry(sexp, ORDER_REPLY_MAX_ATTEMPTS, ORDER_REPLY_RETRY_DELAY_MS, attempts_used);
   if(!sent) {
      LogError("Failed to send ORDER_ACK id=" + id +
               " ticket=" + StringFormat("%I64u", ticket) +
               " attempts=" + IntegerToString(attempts_used));
   } else {
      LogDebug("📨 ORDER_ACK sent id=" + id +
               " ticket=" + StringFormat("%I64u", ticket) +
               " attempts=" + IntegerToString(attempts_used));
   }
}

void SendTradeReject(string orig_cmd, string reason, long retcode) {
   if(!g_pub_connected) return;
   string id = GetStringFromSexp(orig_cmd, "id");
   string norm_reason = reason;
   if(norm_reason == "") norm_reason = "ORDER_REJECT";
   string sexp = StringFormat("((type . \"ORDER_REJECT\") (id . \"%s\") (symbol . \"%s\") (reason . \"%s\") (retcode . %d))",
                              EscapeSexpString(id),
                              EscapeSexpString(_Symbol),
                              EscapeSexpString(norm_reason),
                              (int)retcode);
   int attempts_used = 0;
   bool sent = SendPubWithRetry(sexp, ORDER_REPLY_MAX_ATTEMPTS, ORDER_REPLY_RETRY_DELAY_MS, attempts_used);
   if(!sent) {
      LogError("Failed to send ORDER_REJECT id=" + id +
               " reason=" + norm_reason +
               " attempts=" + IntegerToString(attempts_used));
   } else {
      LogInfo("📨 ORDER_REJECT sent id=" + id +
              " reason=" + norm_reason +
              " retcode=" + IntegerToString((int)retcode) +
              " attempts=" + IntegerToString(attempts_used));
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
      string sexp = StringFormat("((type . \"TICK\") (symbol . \"%s\") (bid . %.5f) (ask . %.5f))",
                                 EscapeSexpString(_Symbol), bid, ask);
      uchar data_snd[];
      StringToCharArray(sexp, data_snd);
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

void DrainExecQueue(int max_commands) {
   if(!g_sub_connected || max_commands <= 0) return;
   for(int i = 0; i < max_commands; i++) {
      uchar data_rcv[8192];
      int size = zmq_recv(g_sub_socket, data_rcv, 8192, ZMQ_DONTWAIT);
      if(size <= 0) {
         break;
      }
      ExecuteCommand(CharArrayToString(data_rcv, 0, size));
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

   // Process queued commands first so queued HEARTBEAT updates freshness before dead-man check.
   DrainExecQueue(MAX_COMMANDS_PER_TIMER);
   
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
         LogInfo("💀 DEAD MAN'S SWITCH! Brain silent >" + IntegerToString(HEARTBEAT_TIMEOUT) + "s. EMERGENCY CLOSE " + _Symbol);
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
}
//+------------------------------------------------------------------+

//+------------------------------------------------------------------+
//| V19: Send open positions for allocation sync                      |
//+------------------------------------------------------------------+
void SendOpenPositions() {
   if(!g_pub_connected) return;
   
   string sexp = "((type . \"POSITIONS\") (symbol . \"" + EscapeSexpString(_Symbol) + "\") (data . (";
   bool first = true;
   int pos_count = 0;
   
   for(int i = PositionsTotal() - 1; i >= 0; i--) {
      ulong ticket = PositionGetTicket(i);
      if(ticket > 0 && PositionSelectByTicket(ticket)) {
         // Only include positions for THIS symbol with Swimmy magic
         if(PositionGetString(POSITION_SYMBOL) == _Symbol && 
            PositionGetInteger(POSITION_MAGIC) >= MAGIC_BASE) {
            if(!first) sexp += " ";
            first = false;
            pos_count++;
            
            double entry_price = PositionGetDouble(POSITION_PRICE_OPEN);
            sexp += StringFormat("((ticket . %I64u) (magic . %I64d) (type . \"%s\") (volume . %.2f) (entry_price . %.5f))",
               ticket,
               PositionGetInteger(POSITION_MAGIC),
               (PositionGetInteger(POSITION_TYPE) == POSITION_TYPE_BUY) ? "BUY" : "SELL",
               PositionGetDouble(POSITION_VOLUME),
               entry_price);
         }
      }
   }
   sexp += "))";
   
   uchar data[];
   StringToCharArray(sexp, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   LogInfo("📊 POSITIONS sent: " + IntegerToString(pos_count) + " positions for " + _Symbol);
}

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
   
   string sexp = StringFormat(
       "((type . \"ACCOUNT_INFO\") (timestamp . %d) (equity . %.2f) (balance . %.2f) (margin . %.2f) (free_margin . %.2f) (margin_level . %.2f) (profit . %.2f) (leverage . %d))",
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
   StringToCharArray(sexp, data);
   zmq_send(g_pub_socket, data, ArraySize(data)-1, ZMQ_DONTWAIT);
   
   LogDebug("[ACCOUNT] Equity=" + DoubleToString(AccountInfoDouble(ACCOUNT_EQUITY), 0));
}
