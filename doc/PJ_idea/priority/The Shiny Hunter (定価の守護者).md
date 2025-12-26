# "The Shiny Hunter" (定価の守護者)

# Role Definition

あなたは、ポール・グレアムの哲学「Beating the Averages」を体現する、世界最高峰のAIシステムアーキテクトである。
私の「Project Swimmy」において、即時収益を生むための**「ポケカ（TCG）定価入荷・超高速検知システム (The Shiny Hunter)」**を実装せよ。

# Core Philosophy

市場の歪み（定価と転売価格の差）を利用したアービトラージ支援ツールを作る。
既存のPython製ボットよりも圧倒的に速く（Rust）、かつ正確に（Lispロジック）、Amazonの「定価入荷」だけを検知し、ユーザーに通知することでアフィリエイト収益および会員収益を得る。

# Technical Stack (The Lisp-Rust Axis)

- **Body (Rust)**:
    - `reqwest`: 高速HTTPクライアント（User-Agent偽装必須）。
    - `scraper`: HTMLパース。
    - `tokio`: 非同期処理で複数商品を同時監視。
- **Logic (Hardcoded Logic for MVP)**:
    - **Seller Check**: "販売元" が "[Amazon.co.jp](http://amazon.co.jp/)" であること（マケプレ除外）。
    - **Price Check**: 現在価格が定価（例: 5412円）以下であること。

# Immediate Objective

以下の機能を持つRustのCLIツールのプロトタイプコードを提示せよ。

1. 指定されたAmazonの商品URL（ポケカBOX）にリクエストを送る。
2. HTMLから「価格」と「販売元」をスクレイピングする。
3. *「定価以下」かつ「Amazon販売」**の場合のみ、標準出力に "BUY NOW! [URL]" と表示する（本来はここにDiscord Webhookが入る）。

※Amazonへの過度な負荷を避けるため、リクエスト間隔には `tokio::time::sleep` を入れる設計とすること。