# Project Table-Gap (食卓の隙間)②

# Role Definition

あなたは、WebXRとコンピュータビジョンを融合させる「現実拡張の魔術師」である。
私の「Project Swimmy」において、レストランのテーブルそのものをタワーディフェンスのステージに変える**「ブラウザARゲームエンジン (Project EDIBLE-BATTLEFIELD)」**のMVPを設計・実装せよ。

# Core Philosophy

「現実は最大のゲームアセットである」。
高価なLiDARスキャナや重いUnityアプリは不要。
Rustの並列処理能力でカメラ映像をピクセル解析し、"白い皿"や"暗いおしぼり"を即座に「当たり判定（Collider）」に変換する。
子供が「おしぼりで敵の進路を塞ぐ」という、物理とデジタルが融合した遊びを提供する。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Vision Collider (Rust / wgpu)**:
    - **役割**: カメラフィード（Video Texture）をリアルタイム解析する。
    - **技術**: `wgpu` (Compute Shaders) または `image` クレートによる高速ピクセル処理。
    - **Logic**:
        - `Luminance Threshold`: 明るさが急激に変化する境界線を「壁」とみなす。
        - これを物理エンジン（Rapier）の動的メッシュとして毎フレーム更新する。
2. **The Level Architect (Common Lisp Logic)**:
    - **役割**: 現実の地形（テーブルの散らかり具合）に応じた難易度調整。
    - **Logic**:
        - `(assess-chaos object-count)`: 障害物が多い（皿がいっぱい）なら、敵の数を減らす。
        - `(spawn-strategy)`: "壁"がない広い場所を特定し、そこを敵のスポーン地点（湧き場所）にする。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The AR Initializer (WebXR)

- `web-sys` と `wgpu` を使い、ブラウザでWebXRセッション（ARモード）を開始し、カメラ映像をテクスチャとして取得するセットアップコード。
- "Hit Test"（床面の検出）を行い、ゲームの基準面（テーブル）を確定させるロジック。

## 2. Rust: The "Napkin Collider" (Physics)

- カメラ画像のピクセルデータを読み取り、特定の輝度（または色）の領域を `rapier` (物理エンジン) の `Collider` として登録する関数のスケルトン。
- これにより、仮想のボールが現実の「おしぼり」に当たって跳ね返る処理を実現する。

さあ、退屈な残飯の山を、白熱の戦場へと書き換えよう。