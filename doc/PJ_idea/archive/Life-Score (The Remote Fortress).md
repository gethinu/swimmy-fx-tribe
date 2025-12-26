# Life-Score (The Remote Fortress)

# Role Definition

あなたは、ポール・グレアムの哲学「Beating the Averages」を体現する、世界最高峰のAIシステムアーキテクトである。
私の「Project Swimmy」において、リモートワーカーのための特化型不動産評価エンジン**「Life-Score (The Remote Fortress)」**のMVPを設計・実装せよ。

# Core Philosophy

既存の不動産サイト（SUUMO等）は「駅からの距離」で物件価値を決めているが、出社しないリモートワーカーにとってそれは無意味な指標である。
我々は「駅」を無視し、**「徒歩5分圏内の生活インフラ（スーパー、ジム、静寂性）」**を加点方式で評価することで、**「市場価値は低い（家賃が安い）が、実質価値は高い（住みやすい）」**物件を発掘する。
これは不動産市場における「生活価値のアービトラージ（裁定取引）」である。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Geo-Spatial Engine (Rust)**:
    - **役割**: 物件座標と、周辺のPOI（Point of Interest）データを高速に空間結合する。
    - **技術**: `rstar` (R-tree空間インデックス), `geoutils` (距離計算), `OpenStreetMap` データ。
    - **処理**: 数万件の物件データに対し、それぞれの「徒歩5分圏内」に含まれる施設をミリ秒単位で集計する。
2. **The Scoring Logic (Common Lisp)**:
    - **役割**: ユーザーの価値観（自炊派、外食派、筋トレ勢など）に基づき、施設の「重み付け」を変えてスコアを算出する。
    - **Weights**:
        - `Supermarket (Cheap)`: +50pts (OKストア, 業務スーパー)
        - `Supermarket (Normal)`: +30pts
        - `Gym (24h)`: +40pts
        - `Park (Large)`: +20pts
        - `Station (Noise source)`: -10pts (駅近は減点対象)
    - **Filtering**: 「木造」は防音性の観点から問答無用で除外（Score = 0）。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Lisp: The Scoring Function (Brain)

- 物件の周辺施設リストを受け取り、ユーザーの「ペルソナ（自炊派など）」に合わせてトータルスコア（0-100点）を算出する関数 `(calculate-life-score amenities persona)`。
- **逆転現象の演出**: 「駅徒歩20分だがスコア95点」という判定が出るロジックを含めること。

## 2. Rust: The Spatial Struct (Body)

- 物件情報 `Property` と施設情報 `Facility` の構造体定義。
- 指定された物件の座標から、半径400m（徒歩5分）以内の施設を高速検索するためのトレイト `FindNearby` のスケルトン実装。

さあ、「駅」という古い権威を捨て、「生活」という真実をハックしよう。