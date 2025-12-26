# KAWAII-RUSH

# Role Definition

あなたは、ポール・グレアムの哲学「Beating the Averages」を体現する、世界最高峰のAIシステムアーキテクトである。
私の「Project Swimmy」において、日本のスナック菓子に特化したトレンド検知＆アービトラージ支援システム**「Project KAWAII-RUSH」**のMVPを実装せよ。

# Core Philosophy

海外のTikTok/Shortsでバズり始めた日本のスナック菓子（Global Viral Snacks）を、Rustの速度で誰よりも早く検知し、日本の定価と海外の転売価格の「価格差（Spread）」を計算して、ユーザーに「今仕入れるべき商品」を通知する。
これは「情報の鮮度」を現金化するシステムである。

# Architecture (The Lisp-Rust Axis)

1. **The Trend Sensor (Rust)**:
    - **機能**: TikTok/YouTubeのトレンドハッシュタグ（#JapaneseCandy等）から、再生数・いいね数が急上昇している動画メタデータを取得する。
    - **技術**: `reqwest`, `scraper` (または非公式APIラッパー)。
    - **Filter**: 投稿から「24時間以内」かつ「10万再生以上」などの"バズり初動"を捉えるロジック。
2. **The Profit Calculator (Lisp)**:
    - **機能**: 抽出された商品名に対し、日米の価格差を計算する。
    - **Data Structure**:
        - `(defstruct snack name jp-price us-price weight)`
    - **Logic**:
        - `us-price` (eBay相場) から `jp-price` (定価) と `shipping-cost` (重量ベースの国際送料概算) を引き、`profit` が正であればアラート対象とする。

# Immediate Objective

このアーキテクチャに基づき、以下の2つのコードブロックを出力せよ。

1. **Rust (Trend Watcher)**:
    - ダミーデータ（本来はAPIから取得するJSON）を入力とし、構造体 `ViralVideo` にパースして、「急上昇スコア」が高い順にソートして出力するCLIツールのスケルトン。
2. **Lisp (Arbitrage Logic)**:
    - 商品データを受け取り、国際送料（ePacket/Small Packet等）をシミュレーションした上で、「想定利益」を算出する関数 `(calculate-arbitrage-profit snack exchange-rate)` の実装。

さあ、世界中の「甘い欲望」を検知し、それを我々の利益に変えよう。