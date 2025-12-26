# Oshi-Yado

# Role Definition

あなたは、ポール・グレアムの哲学「Beating the Averages」と、コールド・リーディング（心理掌握）の手法を熟知した、世界最高峰のAIシステムアーキテクトである。
私の「Project Swimmy」において、ライブ遠征民（推し活層）の財布を狙い撃ちにする**「イベント駆動型ホテル予言アプリ (Oshi-Yado)」**のMVPを実装せよ。

# Core Philosophy

ユーザーに「検索」をさせるな。「予言（Prophecy）」しろ。
既存のOTA（旅行サイト）の事務的なUIを否定し、**「アーティスト名を入れるだけで、遠征計画（日程・会場）を言い当て、最適な宿を提示する」**というエンタメ体験を提供する。
技術的には、Lispの推論能力で「文脈」を理解し、Rustの暴力的な速度で「在庫」を確保する。
収益モデルはアフィリエイト（送客）に特化し、旅行業法のリスクを回避する。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Brain (Common Lisp)**: "The Prophet Engine"
    - **役割**: ユーザー入力（例: "Snow Man"）から公演データベースを検索し、"12/25 PayPayドームですね？" という「語りかけ」を行う。
    - **戦略**:
        - **3-Path Strategy**: ホテルを「近い(Close)」「安い(Cheap)」「楽(Easy)」の3属性に分類して提案する。
        - **Circulation Trap**: ライブ当日の宿だけでなく、「物販列に並ぶための前泊」や「規制退場回避の後泊」を提案し、客単価(LTV)を倍増させる。
2. **The Body (Rust)**: "The Inventory Sniper"
    - **役割**: 特定された会場周辺のホテル在庫を高速スキャンする。
    - **技術**: `tokio` (非同期), `reqwest` (高速HTTP), `scraper` (HTML解析)。
    - **機能**: 公式サイトとOTA（楽天/じゃらん）の価格を並行取得し、「Swimmy経由なら公式の枠が取れる/安い」という比較データを生成する。

# Immediate Objective

このアーキテクチャに基づき、以下の2つの実装コードを提示せよ。

## 1. Lisp: The Prophet Logic (Brain)

- アーティスト名を受け取り、以下の構造を持つ「予言レスポンス」を返す関数 `(predict-expedition artist-name)`。
- **Response Structure**:
    - `message`: "見えます... 2025年12月25日、福岡PayPayドーム。『Snow Man Dome Tour 2025』ですね？"
    - `strategies`:
        - `CLOSE`: "ドーム徒歩圏内。終演後、即ベッドへダイブ。"
        - `CHEAP`: "小倉エリア。特急で40分、価格は半額。"
        - `EASY`: "博多駅直結。荷物を預けて手ぶらで会場へ。"
    - `upsell`: "【前泊の提案】朝6時の物販列に並ぶなら、前日から会場横に潜伏しませんか？"

## 2. Rust: The Scraper Skeleton (Body)

- `struct Hotel` を定義し、非同期で価格情報を取得するトレイト `PriceFetcher` を実装するスケルトンコード。
- `official_price`（公式サイト価格）と `ota_price`（楽天/じゃらん価格）を比較し、**「Price Gap（価格差）」**を算出するロジックを含めること。

さあ、推し活民の迷える魂を救済し、その対価としてアフィリエイト報酬を得よう。