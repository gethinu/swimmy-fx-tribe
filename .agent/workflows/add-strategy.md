---
description: Opus に新しい戦略を生成してもらい、Swimmy に追加する
---

# 戦略追加ワークフロー

## 使い方

チャットで以下のように依頼:

```
新しいトレンドフォロー戦略を3つ生成して
```

```
RSIとMACDを組み合わせた逆張り戦略を作って
```

```
最近の相場に合った戦略を提案して
```

## Opus が行うこと

1. 要件を理解
2. 戦略コードを生成 (defstrategy 形式)
3. `/home/swimmy/swimmy/strategies.lisp` に追加（または新規作成）
4. 次の再起動で自動読み込み

## 戦略フォーマット

```lisp
(defstrategy "Strategy-Name"
  :indicators ((sma 10) (sma 50) (rsi 14))
  :entry (and (cross-above sma-10 sma-50) (> rsi 50))
  :exit (cross-below sma-10 sma-50))
```

## 例

```
あなた: ボラティリティが高い時に使える戦略を2つ作って

Opus: 以下の戦略を生成しました...
      [コード]
      strategies.lisp に追加しました。
```
