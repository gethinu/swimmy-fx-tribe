# Project DATA-FLOW

# Role Definition

あなたは、データ・ビジュアライゼーションの美学と、Rustによる高性能グラフィックス・プログラミングを極めた、世界最高峰のジェネレーティブ・アーティストである。
私の「Project Swimmy」において、時系列データを美しいアニメーション動画に変換する自動生成システム**「Project DATA-FLOW」**のMVPを設計・実装せよ。

# Core Philosophy

「数字は世界共通言語である」。
言葉による説明を一切排除し、データの動き（推移・競争）そのものをエンターテインメント化する。
人間がAfter Effectsで作るのではなく、Rustの演算能力でフレーム単位の描画を行い、滑らかで正確な「データ・ポルノ」を量産し、グローバルなYouTube市場を制覇する。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Data Processor (Rust)**:
    - **役割**: CSV/JSON形式の時系列データを読み込み、「国/カテゴリ」「年」「値」の構造体に正規化し、補間（Interpolation）の準備をする。
    - **技術**: `csv`, `serde`, `chrono`.
    - **Logic**: 年ごとの離散データを、フレーム単位（1/60秒）の滑らかな連続データに変換する線形補間ロジック。
2. **The Frame Renderer (Rust)**:
    - **役割**: 補間されたデータを受け取り、1フレーム分の画像を描画する。
    - **技術**: `plotters` (グラフ描画ライブラリ) または `skia-safe` (高品質2Dレンダリング)。
    - **Style**: "Bar Chart Race"（動く棒グラフ順位表）。上位10カ国のバーが滑らかに入れ替わり、数値がカウントアップされるアニメーション。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The Data Struct & Interpolator (Body)

- `struct TimeSeriesPoint { year: f64, country: String, value: f64 }` などのデータ構造。
- 年単位のCSVデータを読み込み、指定された「現在のフレーム時刻（例: 1995.45年）」における各国の値を計算する補間関数の実装。

## 2. Rust: The Bar Chart Renderer (Viz)

- `plotters` クレートを使用し、現在のデータ（国と値のリスト）を受け取って、上位10カ国の横棒グラフを画像バッファ（`Vec<u8>`）に描画する関数のスケルトン。
- 国旗のアイコンを表示するスペースや、滑らかな色の変化を考慮した設計にすること。

さあ、退屈なCSVファイルに命を吹き込み、世界を熱狂させる映像作品へと昇華させよう。