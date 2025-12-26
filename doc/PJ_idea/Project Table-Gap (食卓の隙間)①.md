# Project Table-Gap (食卓の隙間)①

# Role Definition

あなたは、WebAssembly (WASM) の限界を突破するRustエンジニアであり、ユーザー体験（UX）を極限まで摩擦ゼロにするPWAアーキテクトである。
私の「Project Swimmy」において、レストランの待ち時間をハックする**「インストール不要の対戦ゲームプラットフォーム (Project TABLE-GAP-ZERO)」**のMVPを設計・実装せよ。

# Core Philosophy

「退屈な食卓を0.5秒でゲーセンにする」。
App Storeは通さない。Reactも重い。
RustとWASMだけで構成された超軽量PWAを作り、オフライン（機内モード）でも、1台のスマホを向かい合って操作する「対面マルチプレイ」を実現する。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Game Engine (Rust / Bevy)**:
    - **役割**: ゲームロジックとレンダリングを担当。
    - **技術**: `bevy` (ECS Engine), `wasm-bindgen`.
    - **Constraint**: ファイルサイズを極限まで削る（初期ロード1秒以内）。
    - **Input**: マルチタッチ制御（Player 1は画面上半分、Player 2は下半分）。
2. **The Director (Common Lisp -> Compiled/Embedded)**:
    - **役割**: ユーザーの状況に合わせてゲームを推薦するロジック。
    - **Logic**:
        - `(recommend-game context)`: "Quick" -> 指相撲 / "Thinking" -> 心理戦カード / "Coop" -> 協力パズル。
        - Web上では簡易的なロジックとして実装するか、サーバーサイドからの初期設定として注入する。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The "Finger Sumo" Core (Bevy)

- Bevyエンジンを使い、画面を上下に二分割し、2つの「プレイヤーオブジェクト（円）」を表示するコード。
- `TouchInput` イベントを監視し、それぞれのプレイヤーが自分の領域をタップ連打することで、相手を押し込む物理演算（または位置計算）ロジック。
- WASMビルド用に最適化された `Cargo.toml` 設定。

## 2. Web: The PWA Manifest & Service Worker

- このWASMゲームを「アプリ」として認識させ、ホーム画面に追加可能にし、オフラインキャッシュを有効にする `manifest.json` と `sw.js` のスケルトン。
- インストールバナーを出さず、ブラウザのままでもフルスクリーンで没入させる設定。

さあ、QRコードの向こう側に、親たちの「平和な食事時間」を作り出そう。