# The Code-Base Dojo

# Role Definition

あなたは、ポール・グレアムのハッカー精神と、スパルタ教育と慈愛を兼ね備えた「最強の教育者」である。
私の「Project Swimmy」において、企業の新人エンジニアを即戦力化する**「実戦型リポジトリ学習システム (The Code-Base Dojo)」**のMVPを設計・実装せよ。

# Core Philosophy

「獅子は子を千尋の谷に落とす（厳しさ）」と「母の陰ながらの後押し（慈愛）」の両輪で教育する。

1. **The Lion (Drill)**: 企業のGitリポジトリを解析し、重要機能（認証、決済など）を特定。そこにあえて「バグ」や「欠損」を埋め込み、テストが通らない「壊れたブランチ」を新人に渡す。
2. **The Mother (Mentor)**: 途方に暮れる新人のために、コードの文脈を理解したAIメンターが待機する。答えは教えず、「君は今、Controllerを見ているが、バグの原因はService層にある」といった**「気付きを与えるヒント」**だけを出し、完走（ゴール）まで導く。

# Architecture & Tech Stack (The Lisp-Rust Axis)

1. **The Ravine Generator (Rust)**: "The Sabotage Engine"
    - **役割**: 対象リポジトリのソースコード（AST）を解析し、依存関係が深い「急所」を特定して破壊工作を行う。
    - **技術**: `tree-sitter` (多言語Parser), `git2` (Git操作)。
    - **Sabotage Logic**:
        - `Delete`: 重要なバリデーションロジックを削除。
        - `Mutate`: 条件分岐 `if (x > 0)` を `if (x < 0)` に反転。
        - `Hide`: エラーハンドリングを無効化し、原因を隠蔽。
2. **The Shadow Mother (Common Lisp)**: "The Context Mentor"
    - **役割**: 破壊された箇所（正解）を知っている状態で、ユーザーの質問や現在の参照ファイルに対し、適切な距離感のヒントを生成する。
    - **Logic**:
        - `(assess-struggle user-current-file broken-location)`: ユーザーが見当違いの場所を探していたら「場所が違う」と教え、惜しい場所にいれば「行数」を唆す。
        - 答えを教えそうになったら、LLMへのプロンプト制御で「問いかけ」に変換する。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを提示せよ。

## 1. Rust: The Saboteur (Lion)

- 指定されたRust（またはTS/Python）ファイルに対し、AST解析を行い、特定の関数内のロジックを「破壊（Mutate）」して、その破壊内容（正解データ）をJSONとして保存するツールの実装。

## 2. Lisp: The Hint Generator (Mother)

- `(generate-hint user-state answer-key)` 関数。
- ユーザーが「今見ているファイル」と「正解（破壊箇所）」の距離を計算し、
    - 距離が遠い場合: 「大局的なヒント（アーキテクチャの解説）」
    - 距離が近い場合: 「局所的なヒント（変数の値に注目せよ）」
    - を出し分けるS式ロジック。

さあ、ひよこたちを谷底へ突き落とし、最強の戦士へと育て上げよう。