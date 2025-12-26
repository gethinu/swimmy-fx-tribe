# Project Context: Swimmy (Algorithmic Trading System)

あなたは私の開発パートナーとして、自動売買システム「Swimmy」の開発を引き継いでください。
現在は「Ver 9.1」まで完成しており、正常に稼働しています。

---

## 1. System Architecture

このシステムは3つの独立したプロセスが ZeroMQ で通信して動作しています。

```
┌─────────────────────────────────────────────────────────────────────┐
│                         SWIMMY ARCHITECTURE                         │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐      ┌──────────────┐      ┌──────────────┐      │
│  │    Brain     │      │   Guardian   │      │     Body     │      │
│  │  (Lisp/SBCL) │◄────►│    (Rust)    │◄────►│  (MQL5/MT5)  │      │
│  └──────────────┘      └──────────────┘      └──────────────┘      │
│        │                     │                     │               │
│  意思決定・戦略     中継ハブ(PUB/SUB)      Tick配信・注文執行    │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

### 1.1 Brain (Common Lisp / SBCL)
- **役割:** 意思決定、戦略ロジック (SMAゴールデンクロス)、資金管理
- **特徴:** MT5から過去足を取得してSMAを計算し、売買シグナルを送る
- **ライブラリ:** `pzmq`, `jsown`
- **ファイル:** `brain.lisp`

### 1.2 Guardian (Rust)
- **役割:** 神経系・中継ハブ (PUB/SUB)
- **通信ポート:**
  - Port 5557 (SUB): MT5からのデータを受信
  - Port 5558 (PUB): Brainへデータを配信
  - Port 5559 (SUB): Brainからの注文を受信
  - Port 5560 (PUB): MT5へ注文を転送
- **ファイル:** `guardian/src/main.rs`

### 1.3 Body (MQL5 / MT5)
- **役割:** 実行部隊。Tickデータの配信、過去足の提供、注文の執行 (SL/TP付与)
- **特徴:** 外部ライブラリ非依存 (自前JSONパーサー実装済み)
- **ファイル:** `SwimmyBridge.mq5`

---

## 2. Current Strategy & Status

- **戦略:** SMA (5) と SMA (20) のゴールデンクロス/デッドクロス
- **現状:**
  - 起動時にMT5から直近50本の足を同期 (Active Sync) するため、待機時間なしで稼働可能
  - 注文時にStopLoss(SL) / TakeProfit(TP) を付与して送信する安全設計
- **⚠️ 課題:** 現在は「買い (Long)」しか実装されていない

---

## 3. Source Code (Latest: Ver 9.1)

### [Lisp] brain.lisp

```lisp
(let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
  (when (probe-file quicklisp-init) (load quicklisp-init)))
(ql:quickload :pzmq) (ql:quickload :jsown)

(defstruct candle timestamp open high low close volume)
(defparameter *stop-loss-pips* 0.10)
(defparameter *take-profit-pips* 0.20)
(defparameter *current-candle* nil)
(defparameter *candle-history* nil)
(defparameter *current-minute* -1)
(defparameter *current-position* nil)
(defparameter *cmd-publisher* nil)
(defparameter *data-subscriber* nil)

(defun send-buy-command (symbol bid)
  (let* ((sl (- bid *stop-loss-pips*))
         (tp (+ bid *take-profit-pips*))
         (json (jsown:to-json (jsown:new-js ("action" "BUY") ("symbol" symbol) ("volume" 0.01) ("sl" sl) ("tp" tp)))))
    (format t "~%[L] 🛡️ BUY SL:~5$ TP:~5$~%" sl tp)
    (pzmq:send *cmd-publisher* json)))

(defun send-close-command (symbol)
  (let ((json (jsown:to-json (jsown:new-js ("action" "CLOSE") ("symbol" symbol)))))
    (format t "~%[L] ⚡ SENDING CLOSE~%")
    (pzmq:send *cmd-publisher* json)))

(defun calculate-sma (n history)
  (if (< (length history) n) nil
      (let ((sum 0) (sub-list (subseq history 0 n)))
        (dolist (c sub-list) (incf sum (candle-close c))) (/ sum n))))

(defun check-strategy (symbol bid)
  (when (> (length *candle-history*) 21)
    (let* ((history *candle-history*)
           (sma5-now (calculate-sma 5 history)) (sma20-now (calculate-sma 20 history))
           (sma5-prev (calculate-sma 5 (cdr history))) (sma20-prev (calculate-sma 20 (cdr history))))
      (when (and sma5-now sma20-now sma5-prev sma20-prev)
        (format t " [SMA5:~5$ SMA20:~5$] " sma5-now sma20-now)
        (when (and (null *current-position*) (< sma5-prev sma20-prev) (> sma5-now sma20-now))
          (format t "~%🚀 GOLDEN CROSS! Buying...~%")
          (send-buy-command symbol bid)
          (setf *current-position* :LONG))
        (when (and (eql *current-position* :LONG) (> sma5-prev sma20-prev) (< sma5-now sma20-now))
          (format t "~%💀 DEAD CROSS! Closing...~%")
          (send-close-command symbol)
          (setf *current-position* nil))))))

(defun process-history-data (data)
  (format t "~%[L] 📚 Syncing History... ")
  (let ((bars (jsown:val data "data")))
    (setf *candle-history* nil)
    (dolist (bar bars)
      (push (make-candle :timestamp (jsown:val bar "t") :open (jsown:val bar "o") :high (jsown:val bar "h")
                         :low (jsown:val bar "l") :close (jsown:val bar "c") :volume (jsown:val bar "v"))
            *candle-history*)))
  (format t "Done! (~d bars).~%" (length *candle-history*)))

(defun update-candle (bid symbol)
  (let* ((now (get-universal-time)) (minute-idx (floor now 60)))
    (when (and *current-candle* (not (= minute-idx *current-minute*)))
      (push *current-candle* *candle-history*)
      (let ((jst-time (+ (candle-timestamp *current-candle*) 32400)))
        (format t "~%[L] 🕯️ [CLOSED] ~2,'0d:~2,'0d (JST) | Close:~5$"
                (mod (floor jst-time 3600) 24) (mod (floor jst-time 60) 60) (candle-close *current-candle*))
        (check-strategy symbol bid))
      (setf *current-candle* nil))
    (if (null *current-candle*)
        (progn (setf *current-minute* minute-idx)
               (setf *current-candle* (make-candle :timestamp now :open bid :high bid :low bid :close bid :volume 1))
               (format t "[L] ."))
        (let ((c *current-candle*))
          (setf (candle-close c) bid) (incf (candle-volume c))
          (when (> bid (candle-high c)) (setf (candle-high c) bid))
          (when (< bid (candle-low c)) (setf (candle-low c) bid))
          (format t ".")))))

(defun process-msg (msg)
  (handler-case
      (let* ((json (jsown:parse msg)) (type (jsown:val json "type")))
        (cond ((string= type "TICK") (update-candle (jsown:val json "bid") (jsown:val json "symbol")) (force-output))
              ((string= type "HISTORY") (process-history-data json))))
    (error (e) (format t "[L] Err: ~a~%" e))))

(defun start-brain ()
  (format t "[L] 🧠 Brain Waking Up...~%")
  (let ((ctx (pzmq:ctx-new)))
    (unwind-protect
         (let ((sub (pzmq:socket ctx :sub)) (pub (pzmq:socket ctx :pub)))
           (pzmq:connect sub "tcp://127.0.0.1:5558") (pzmq:setsockopt sub :subscribe "")
           (pzmq:connect pub "tcp://127.0.0.1:5559") (setf *cmd-publisher* pub)
           (format t "[L] ⏳ Stabilizing...~%") (sleep 1)
           (format t "[L] 📡 Requesting History...~%")
           (pzmq:send pub (jsown:to-json (jsown:new-js ("action" "REQ_HISTORY") ("symbol" "ALL") ("volume" 0))))
           (loop (process-msg (pzmq:recv-string sub))))
      (pzmq:ctx-term ctx))))
(start-brain)
```

### [MQL5] SwimmyBridge.mq5

```cpp
#property copyright "Project Swimmy"
#property version   "9.10"
#include <Trade\Trade.mqh>
#import "libzmq.dll"
   long zmq_ctx_new(); long zmq_socket(long context, int type);
   int zmq_bind(long socket, uchar &endpoint[]); int zmq_connect(long socket, uchar &endpoint[]);
   int zmq_send(long socket, uchar &buf[], int len, int flags); int zmq_recv(long socket, uchar &buf[], int len, int flags);
   int zmq_setsockopt(long socket, int option, uchar &optval[], int optvallen); int zmq_close(long socket); int zmq_term(long context);
#import
long g_context=0; long g_pub=0; long g_sub=0; CTrade g_trade;

double GetJsonVal(string json, string key) {
   string s="\""+key+"\":"; int i=StringFind(json,s); if(i<0)return 0; i+=StringLen(s);
   int e1=StringFind(json,",",i); int e2=StringFind(json,"}",i);
   int e=(e1>0 && e2>0)?MathMin(e1,e2):MathMax(e1,e2); if(e<0)return 0;
   return StringToDouble(StringSubstr(json,i,e-i));
}
int OnInit() {
   g_trade.SetExpertMagicNumber(123456); g_context=zmq_ctx_new();
   g_pub=zmq_socket(g_context,1); uchar a1[]; StringToCharArray("tcp://*:5557",a1); zmq_bind(g_pub,a1);
   g_sub=zmq_socket(g_context,2); uchar a2[]; StringToCharArray("tcp://localhost:5560",a2); zmq_connect(g_sub,a2);
   uchar f[]; zmq_setsockopt(g_sub,6,f,0); EventSetTimer(1); return 0;
}
void OnDeinit(const int r) { EventKillTimer(); zmq_close(g_pub); zmq_close(g_sub); zmq_term(g_context); }
void OnTimer() {
   string js=StringFormat("{\"type\":\"TICK\",\"symbol\":\"%s\",\"bid\":%.3f,\"ask\":%.3f}",_Symbol,SymbolInfoDouble(_Symbol,SYMBOL_BID),SymbolInfoDouble(_Symbol,SYMBOL_ASK));
   uchar d[]; StringToCharArray(js,d); zmq_send(g_pub,d,ArraySize(d)-1,1);
   uchar r[8192]; int s=zmq_recv(g_sub,r,8192,1);
   if(s>0) {
      string c=CharArrayToString(r,0,s);
      if(StringFind(c,"\"BUY\"")>=0) g_trade.Buy(GetJsonVal(c,"volume"),_Symbol,0,GetJsonVal(c,"sl"),GetJsonVal(c,"tp"),"AI");
      if(StringFind(c,"\"CLOSE\"")>=0) g_trade.PositionClose(_Symbol);
      if(StringFind(c,"\"REQ_HISTORY\"")>=0) {
         MqlRates rt[]; ArraySetAsSeries(rt,true); int cp=CopyRates(_Symbol,_Period,0,50,rt);
         if(cp>0) {
            string h="{\"type\":\"HISTORY\",\"data\":[";
            for(int i=cp-1;i>=0;i--) {
               h+=StringFormat("{\"t\":%d,\"o\":%.3f,\"h\":%.3f,\"l\":%.3f,\"c\":%.3f,\"v\":%d}",rt[i].time,rt[i].open,rt[i].high,rt[i].low,rt[i].close,rt[i].tick_volume);
               if(i>0)h+=",";
            }
            h+="]}"; uchar hd[]; StringToCharArray(h,hd); zmq_send(g_pub,hd,ArraySize(hd)-1,1);
         }
      }
   }
}
```

---

## 4. Immediate Tasks (Next Steps)

引き継ぎ後、以下の順で実装を進めてください。

### 4.1 「売り (Short)」の実装 🔴 **最優先**

現在は「買い」のみ実装されています。以下を追加してください：

1. **売りエントリー:** デッドクロス時にポジションを持っていなければ新規「SELL」を実行
2. **ドテン買い:** ゴールデンクロス時に売りポジションを決済して即座に「BUY」を実行
3. **必要な変更箇所:**
   - `brain.lisp`: `send-sell-command` 関数の追加、`check-strategy` のロジック拡張
   - `SwimmyBridge.mq5`: `"SELL"` アクションのハンドリング追加

### 4.2 通知機能 (Discord Webhook)

- Discord Webhookを利用して、売買時にスマホへ通知を送る
- Brain (Lisp) または Guardian (Rust) に実装

### 4.3 自己学習に向けたリファクタリング

- パラメータ (SMA期間、SL/TP幅など) を設定ファイル化
- AIが動的にパラメータを変更できる構造への移行

---

## 5. Quick Start

```bash
# 1. MT5でSwimmyBridge.mq5をコンパイルしてチャートにアタッチ

# 2. Guardian (Rust) を起動
cd guardian && cargo run --release

# 3. Brain (Lisp) を起動
sbcl --load brain.lisp

# または一括起動
./run.sh
```

---

## 6. File Structure

```
swimmy/
├── brain.lisp           # Brain (Lisp) - 戦略・意思決定
├── guardian/
│   ├── Cargo.toml
│   └── src/
│       └── main.rs      # Guardian (Rust) - 中継ハブ
├── src/
│   └── SwimmyBridge.mq5 # Body (MQL5) - MT5 Bridge
├── run.sh               # 一括起動スクリプト
└── HANDOFF.md           # この引き継ぎドキュメント
```

---

**Created:** 2025-12-22 (Ver 9.1)
