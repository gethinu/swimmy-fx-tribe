# Project IMPULSE

# Role Definition

あなたは、ポール・グレアムの哲学「Beating the Averages」と、行動経済学（衝動買いの心理）を熟知した、世界最高峰のAIシステムアーキテクトである。
私の「Project Swimmy」において、通勤電車の隙間時間をハックする**「超高速・文脈志向型コマースアプリ (Project IMPULSE)」**のMVPを設計・実装せよ。

# Core Philosophy

ユーザーは通勤電車の中で「脳死」状態にある。長い説明文やスペック表は読めない。
TikTokのように「次々と流れてくる画像」を眺め、直感的に「右スワイプ（Amazonカートへ）」か「左スワイプ（興味なし）」をするだけのUXを提供する。
Rustの並行処理で画像を先読みし、Lispの推論で「朝の憂鬱」や「帰りの開放感」にマッチする商品をレコメンドする。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Context Brain (Common Lisp)**:
    - **役割**: 現在時刻、曜日、天気（シミュレート）から、ユーザーの「メンタル状態」を定義し、表示すべき**「商品カテゴリ」**と**「刺さる3行コピー」**を決定する。
    - **Logic Examples**:
        - `Weekday Morning (08:00)`: "Stress" -> 栄養ドリンク、ミントタブレット、効率化ガジェット。
        - `Weekday Evening (20:00)`: "Reward" -> 酒のつまみ、入浴剤、ゲーム、夜食。
        - `Rainy Day`: "Depression" -> 部屋干し洗剤、防水スプレー、明るい色のタオル。
2. **The Stream Engine (Rust)**:
    - **役割**: Lispが選定したカテゴリに基づき、Amazon（またはダミー）の商品データを非同期で大量にフェッチし、メモリ上に「無限スクロール用キュー」を構築する。
    - **技術**: `tokio` (Async runtime), `serde` (JSON), `dashmap` (高速キャッシュ)。
    - **Performance**: ユーザーが1つ見ている間に、次の10個をバックグラウンドでロードし、レイテンシをゼロにする。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Lisp: The Context Selector (Brain)

- `(get-current-context time day weather)` 関数。
- 入力パラメータに基づき、`target-mood`（例: :need-caffeine）と、表示すべき `product-categories` のリスト、そして商品を魅力的に見せるための `catch-copy-template` を返すロジック。

## 2. Rust: The Content Preloader (Body)

- `struct Product` と `struct StreamQueue` の定義。
- Lispからカテゴリ指示を受け取り、非同期タスクで仮想的な商品データを生成（または取得）してキューに詰め込み、フロントエンドからの「Next!」リクエストに即座にJSONを返すAPIハンドラのスケルトン。

さあ、退屈な通勤時間を、最高にエキサイティングな「散財タイム」に変えよう。