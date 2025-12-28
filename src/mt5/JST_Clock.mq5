//+------------------------------------------------------------------+
//|                                                    JST_Clock.mq5 |
//|                                  Copyright 2023, Antigravity AI  |
//|                                             https://www.mql5.com |
//+------------------------------------------------------------------+
#property copyright "Antigravity AI"
#property link      "https://www.mql5.com"
#property version   "1.00"
#property indicator_chart_window

//--- indicator buffers
int OnInit()
{
   EventSetTimer(1);
   return(INIT_SUCCEEDED);
}

void OnDeinit(const int reason)
{
   EventKillTimer();
   ObjectsDeleteAll(0, "JST_");
}

int OnCalculate(const int rates_total,
                const int prev_calculated,
                const datetime &time[],
                const double &open[],
                const double &high[],
                const double &low[],
                const double &close[],
                const long &tick_volume[],
                const long &volume[],
                const int &spread[])
{
   UpdateClock();
   return(rates_total);
}

void OnTimer()
{
   UpdateClock();
}

void UpdateClock()
{
   datetime mt5_time = TimeCurrent();
   // MT5の時間はサーバー時間(通常冬GMT+2, 夏GMT+3)
   // 日本はGMT+9固定
   
   // GMTを取得して9時間足す
   datetime gmt = TimeGMT();
   datetime jst = gmt + 9 * 3600;
   
   string label_jst = "[JST] " + TimeToString(jst, TIME_DATE|TIME_MINUTES|TIME_SECONDS);
   string label_mt5 = "[MT5] " + TimeToString(mt5_time, TIME_DATE|TIME_MINUTES|TIME_SECONDS);
   
   CreateOrUpdateLabel("JST_Clock_Line1", label_jst, 10, 20, clrOrangeRed);
   CreateOrUpdateLabel("JST_Clock_Line2", label_mt5, 10, 40, clrDeepSkyBlue);
}

void CreateOrUpdateLabel(string name, string text, int x, int y, color clr)
{
   if(ObjectFind(0, name) < 0)
   {
      ObjectCreate(0, name, OBJ_LABEL, 0, 0, 0);
      ObjectSetInteger(0, name, OBJPROP_XDISTANCE, x);
      ObjectSetInteger(0, name, OBJPROP_YDISTANCE, y);
      ObjectSetInteger(0, name, OBJPROP_CORNER, CORNER_LEFT_UPPER);
      ObjectSetString(0, name, OBJPROP_FONT, "Consolas");
      ObjectSetInteger(0, name, OBJPROP_FONTSIZE, 12);
   }
   ObjectSetString(0, name, OBJPROP_TEXT, text);
   ObjectSetInteger(0, name, OBJPROP_COLOR, clr);
}
