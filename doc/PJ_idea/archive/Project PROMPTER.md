# Project PROMPTER

# Role Definition

あなたは、心理言語学とリアルタイム音声処理を極めた、最強のセールス・コーチングAIである。
私の「Project Swimmy」において、会話の文脈を読み取り、最適な次のセリフを指示するAR/HUDシステム**「Project PROMPTER」**のMVPを設計・実装せよ。

# Core Philosophy

「営業は確率論であり、会話は将棋である」。
グダグダな雑談や、見込みのない相手への粘着はリソースの無駄だ。
Rustで音声を爆速解析し、Lispで会話の「フェーズ（アイスブレイク→ヒアリング→クロージング）」を判定。
ユーザーの思考リソースを使わせず、画面に出た「正解のセリフ」を読ませるだけで成約に導く。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Real-time Ear (Rust)**:
    - **役割**: PCのシステム音声（相手の声）とマイク音声（自分の声）をキャプチャし、テキスト化する。
    - **技術**: `cpal` (Audio input), `whisper-rs` (Local STT), `tauri` (Transparent HUD Overlay).
    - **Latency**: 会話のテンポを崩さないよう、テキスト化から表示までを1秒以内に行う。
2. **The Strategist (Common Lisp)**:
    - **役割**: 会話の進行状況を管理し、戦術を指示する。
    - **Logic**:
        - `(detect-phase conversation-history)`: 現在の会話が「雑談」か「商談」かを判定。
        - `(handle-objection user-input)`: 相手のネガティブワード（高い、忙しい）を検知し、即座に「切り返しトークスクリプト」を選択して表示する。
        - `(calculate-kill-score)`: 脈なし判定が出たら「撤退せよ」と警告する。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The Transparent HUD (Body)

- `Tauri` を使い、マウス操作を透過する（Click-through）半透明のウィンドウを作成するコード。
- `Event Loop` でバックエンドからのテキスト（指示）を受け取り、画面の最前面に字幕のように表示する実装。

## 2. Lisp: The Conversation State Machine (Brain)

- 会話の状態遷移（State Machine）を定義するS式。
- 入力されたテキストから「価格への懸念」や「競合との比較」といったインテント（意図）を抽出し、それに対応する**「キラーフレーズ」**をデータベースから引き出す関数。

さあ、口下手な営業マンの脳に、トップセールスの思考回路をインストールしよう。