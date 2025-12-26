//+------------------------------------------------------------------+
//| SwimmyBridge.mq5 - Historical Data Extension                      |
//| ※既存のSwimmyBridge.mq5のOnInit()に以下を追加してください          |
//+------------------------------------------------------------------+

// === OnInit()に追加するコード ===

void SendHistoricalData()
{
    Print("📊 Sending 1 year of M1 historical data...");
    
    MqlRates rates[];
    ArraySetAsSeries(rates, true);
    
    // 1年分 = 525600分（年間取引時間はもっと少ないが余裕を持って）
    int copied = CopyRates(Symbol(), PERIOD_M1, 0, 100000, rates);
    
    if(copied <= 0)
    {
        Print("❌ Failed to copy rates: ", GetLastError());
        return;
    }
    
    Print("✅ Copied ", copied, " candles");
    
    // JSON構築
    string json = "{\"type\":\"HISTORY\",\"symbol\":\"" + Symbol() + "\",\"candles\":[";
    
    for(int i = 0; i < copied; i++)
    {
        if(i > 0) json += ",";
        json += StringFormat(
            "{\"t\":%d,\"o\":%.5f,\"h\":%.5f,\"l\":%.5f,\"c\":%.5f}",
            (int)rates[i].time,
            rates[i].open,
            rates[i].high,
            rates[i].low,
            rates[i].close
        );
        
        // 10000本ごとに分割送信（ZMQ制限対策）
        if((i + 1) % 10000 == 0 && i < copied - 1)
        {
            json += "]}";
            // ここで送信
            char msg[];
            StringToCharArray(json, msg);
            // pubSocket.send(msg) - 既存のソケットを使用
            
            Print("📤 Sent batch ", (i + 1) / 10000);
            
            // 次のバッチを開始
            json = "{\"type\":\"HISTORY_CONTINUE\",\"symbol\":\"" + Symbol() + "\",\"candles\":[";
        }
    }
    
    json += "]}";
    
    // 最終バッチ送信
    char finalMsg[];
    StringToCharArray(json, finalMsg);
    // pubSocket.send(finalMsg;
    
    Print("✅ Historical data sent: ", copied, " candles");
}

// OnInit()に追加:
// SendHistoricalData();
