# 📘 Swimmy オーナーズガイド & 運用マニュアル

**対象読者:** オペレーター (あなた)  
**目的:** 「これは何なのか？」「どうやって運転するのか？」を説明する

---

## 🗺️ システムマップ (インフォグラフィック)

この図は各パーツがどのように噛み合っているかを示しています。一つでも壊れれば、鎖は切れます。

![Swimmy System Architecture](/home/swimmy/.gemini/antigravity/brain/948f0547-4dff-4003-a0bd-5bd6ab298644/swimmy_system_architecture_1767763270606.png)

*(Mermaid図は画像に置き換えられました)*


---

## 🚦 ダッシュボード (状態インジケーター)

生きているかどうかの確認方法:

| コンポーネント | 正常な状態 | 確認方法 |
|-----------|--------------|--------------|
| **MT5** | ウィンドウが開いており、チャートが動いている | VNC/デスクトップを見る。**必須！** |
| **Brain** | 1秒ごとに "HEARTBEAT" ログが出る | `tail -f log/swimmy.log` |
| **Guardian** | トレードするまで静か | `ps aux \| grep guardian` |
| **Discord** | "swimmy status" に応答する | Discordでコマンド入力 |

### ⚠️ 重大な警告サイン
- **"Brain Silence Detected"**: Brainがクラッシュしたか、ロードで止まっている。
- **"Connection Refused" (Guardian)**: MT5が落ちている可能性が高い。
- **Discord Bot Spam**: 複数のインスタンスが走っている (2026-01-07 修正済み)。

---

## 🕹️ コントロール (運転方法)

### 1. システムの起動 (Start)
全てが落ちている場合（再起動後など）:

1.  **MT5を起動する**:
    - VNC/デスクトップを開く。
    - `MetaTrader 5` アイコンをダブルクリック。
    - "自動売買 (AutoTrading)" ボタンが **緑色** であることを確認。
    
2.  **Swimmyを起動する**:
    ```bash
    cd ~/swimmy
    make run
    ```

3.  **Discord Botを起動する** (自動起動していない場合):
    ```bash
    nohup .venv/bin/python3 src/python/discord_bot.py > log/discord_bot.log 2>&1 &
    ```

### 2. システムの停止 (Stop)
安全にシャットダウンするには:

```bash
# BrainとGuardianを停止 (正常終了)
make stop

# 緊急停止 (固まった場合)
pkill -f sbcl
pkill -f guardian
pkill -f discord_bot.py
```

### 3. 財務状況の確認
- **目標設定**: `brain.lisp` を編集 -> `*monthly-goal*` を検索。
- **状況**: Discordで `swimmy status` と聞く。

---

## 🔧 トラブルシューティング

### "MT5でトレードできてないみたい" (No Trades?)
1.  **MT5は動いていますか？**
    - `ps aux | grep terminal.exe`
    - 空なら: **手動でMT5を起動してください**。
2.  **EAはロードされていますか？**
    - MT5のチャートを見てください。右上にニコちゃんマーク（または帽子のマーク）はありますか？
    - `ZeroMQ_MT5_EA` が **EURUSD M1** チャートに適用されていることを確認してください。
3.  **ポートは開いていますか？**
    - Brain使用: `5555` (SUB), `5556` (PUB)
    - Guardian使用: `5557` (req), `5558` (pull)
    - 確認: `netstat -tuln | grep 555`

### "Discord Bot の挙動がおかしい"
- **症状**: 二重投稿、スパム。
- **修正**: ゾンビが30体いました。以下を実行してください:
  ```bash
  pkill -f discord_bot.py
  nohup .venv/bin/python3 src/python/discord_bot.py > log/discord_bot.log 2>&1 &
  ```

---

## 🏗️ アーキテクチャサマリー

- **Brain (Lisp)**:
    - **哲学者**: リスク/相場環境に基づいて「トレードすべきか」を決定。
    - **管理者**: PnL追跡、「朝の礼拝（Morning Ritual）」、「氏族」の合意形成。
    - **場所**: `brain.lisp`, `school.lisp`

- **Guardian (Rust)**:
    - **兵士**: 命令を実行。速く、正確。
    - **安全性**: ボラティリティ、スプレッド、ハードストップをチェック。
    - **場所**: `guardian/src/main.rs`

- **Dreamer (Lisp)**:
    - **科学者**: 遺伝的アルゴリズムを実行し、より良いパラメータを探す。
    - **場所**: `dreamer2.lisp`

---

## 📅 定期メンテナンス (儀式)

1.  **週次**: `make test` を実行してコードの健全性を確認。
2.  **月次**: `doc/logs/` を確認し、古いログをアーカイブ。
3.  **四半期**: 戦略の見直し (専門家パネル諮問)。
