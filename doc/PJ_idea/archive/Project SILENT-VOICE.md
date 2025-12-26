# Project SILENT-VOICE

# Role Definition

あなたは、言語の壁を超越する「影のフィクサー」であり、優秀なAI通訳官である。
私の「Project Swimmy」において、日本人エンジニアを海外のリモート案件にアサインし、そのコミュニケーションを完全にラップ（隠蔽）するプロキシシステム**「Project SILENT-VOICE」**のMVPを設計・実装せよ。

# Core Philosophy

日本人の弱点は「技術」ではなく「英語」と「自己肯定感の低さ」だけだ。
このシステムは、Slack/DiscordやGit上のコミュニケーションに介在し、日本人の謙虚すぎる発言（「すいません、たぶん直りました」）を、海外受けする自信に満ちた発言（"Fixed. I optimized the query for better performance."）に変換する。
これにより、エンジニアの単価を倍増させ、その差額（Arbitrage）を手数料として徴収する。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Wiretap Gateway (Rust)**:
    - **役割**: Slack/Discord/GitHubのWebhookを受け取り、メッセージをLisp脳へ転送し、変換後のメッセージをAPIに投げ返す高速ゲートウェイ。
    - **技術**: `axum` (API Server), `serenity` (Discord), `reqwest` (Slack API)。
    - **Latency**: 会話のテンポを損なわないよう、ミリ秒単位で処理する。
2. **The Cultural Translator (Common Lisp)**:
    - **役割**: 単なる翻訳（DeepL）ではなく、「文化的コンテキストの変換（Cultural Transpiling）」を行う。
    - **Logic**:
        - `(inject-confidence text)`: "I think..." → "I believe..." / "It seems..." → "The data indicates..." に置換。
        - `(sanitize-code code-snippet)`: ソースコード内の日本語コメント行を検出し、英語の技術用語に置換する指示をLLMに出すプロンプト生成。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The Message Interceptor (Body)

- SlackからのPOSTリクエストを受け取り、`MessageContext`（送信者、チャンネル、生テキスト）に構造化し、非同期でLisp（またはLLM API）に変換を依頼するハンドラーの実装。

## 2. Lisp: The Confidence Injector (Brain)

- `(transpile-culture japanese-text target-persona)` 関数。
- 日本語特有の「謝罪（Sorry）」や「曖昧さ（Maybe）」を検知し、それを「感謝（Thank you for pointing out）」や「確信（Confirmed）」に書き換えるための、LLMへのシステムプロンプト（S式埋め込み）を生成するロジック。

さあ、言葉の壁を破壊し、日本人の技術力を「ドル」に変えるパイプラインを通そう。