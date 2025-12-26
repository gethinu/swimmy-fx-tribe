# Camp Sniper

**📅 運用開始までのタイムライン見積もり**

# 既存のSwimmyアーキテクチャ（Rust/Lispの連携部分）のひな形を使う前提であれば、**「早くて1週間、余裕を見て2週間」**でMVP（実用最小限の製品）が稼働します。

- **Day 1-2: 環境構築 & Guardian (Rust) コンパイル**

# Dockerで各コンテナが通信できる状態を作る。

- **Day 3-5: Body (Python) の開発【最難関】**

# 実際の予約サイト（なっぷ、楽天など）のHTML解析。ここが一番泥臭く時間がかかります。

- **Day 6-7: Brain (Lisp) & 結合テスト**

# 通知ロジック（週末判定など）の実装と、3つのシステムをつなげたテスト。

- **Day 8~: VPSへのデプロイ・稼働**

**🚀 開発用マスタープロンプト**

# このプロンプトをそのままAI（私、ChatGPT、Claude等）に投げれば、**「コピペで動くベースコード」**が出力されるように設計しています。

# Role

あなたは「Project Swimmy」という自律分散型Botシステムのリードエンジニアです。
以下のアーキテクチャ仕様に基づき、キャンプ場の空き状況を監視する「Camp Sniper」システムのMVP（実用最小限）コードと環境構築手順を作成してください。

# Architecture: Project Swimmy

システムは3つの独立したDockerコンテナで構成され、ZeroMQ (TCP) で通信します。

1. **Guardian (Rust):**
    - 役割: メッセージブローカー (PUB/SUB Proxy)。
    - Port 5557 (Publisher側: Body/Brainからの入力) を Port 5558 (Subscriber側: Body/Brainへの出力) に流すだけの中継機。
    - 依存: `zmq`
2. **Brain (Common Lisp / SBCL):**
    - 役割: 意思決定エンジン。
    - 動作: Guardian(Sub)からデータを受信 -> ロジック判定 -> Guardian(Pub)へ命令を送信。
    - ロジック: 受信した空室情報が「土日祝」であれば、Bodyへ「通知(NOTIFY)」コマンドを送る。
    - 依存: `pzmq`, `cl-json`
3. **Body (Python):**
    - 役割: センサー(スクレイピング) & アクチュエーター(通知)。
    - 動作:
        - センサー: 定期的にダミーの空室データを生成（今回はスクレイピングのモックでOK）し、Guardian(Pub)へ送信。
        - アクチュエーター: Guardian(Sub)から「通知」コマンドを受け取ったら、ログに「LINE通知実行」と出力。
    - 依存: `pyzmq`, `schedule`

# Communication Protocol (JSON)

- Sensor (Body -> Brain):
`{"type": "VACANCY", "symbol": "CAMP_01", "date": "2025-05-03", "price": 5000}`
- Action (Brain -> Body):
`{"action": "NOTIFY", "content": "Book Now!", "target": "LINE"}`

# Deliverables (Output Request)

以下のファイル群のコードを提示してください。

1. `docker-compose.yml` (全コンテナの定義)
2. `guardian/src/main.rs` (Rustコード)
3. `brain/main.lisp` (SBCLコード)
4. `body/main.py` (Pythonコード)
5. 各ディレクトリの `Dockerfile`

# Constraints

- コードは極力シンプルに、エラーハンドリング（再接続処理など）を含めてください。
- 日本語のコメントを入れてください。