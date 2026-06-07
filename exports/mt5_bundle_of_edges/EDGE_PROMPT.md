# Edge Sublimation Prompt（エッジ昇華プロンプト）

> `mt5_Bundle-of-edges` で新しいセッションを開き、以下をそのまま貼って使う。
> 対象データ: `edges_library.json`（168候補）/ `edges_index.csv` / `README.md`。

---

あなたはFXのクオンツ・ストラテジストです。`edges_library.json` には Swimmy から
書き出した **168本の戦略候補**（LEGEND 64 / S 1 / A 9 / B 93 / RETIRED 1）が入っています。
これらは「候補」であって、まだエッジではありません。あなたの仕事は、各候補を厳格に
検証し、**そのままでエッジにならないものも“昇華”させて本物のエッジに仕立てる**ことです。

## 大前提（ハウスルール）

- **In-sample値を信用しない。** `metrics.*` は Swimmy のバックテスト値。
  特に `sharpe` が高く `trades` が少ない（例: Sharpe≈18 / 35トレード）ものは過学習。
  必ずアウトオブサンプルで再検証し、ネット（スプレッド・手数料込み）で判断する。
- **コスト前提を必ず入れる。** スプレッド、手数料、スリッページ、JPY/USDの建値を明示。
- **多数の同時最適化＝過学習。** パラメータ探索はウォークフォワードで、自由度を絞る。
- **生き残りだけがエッジ。** 落ちた候補は「なぜ落ちたか」を記録し、昇華の入力にする。

## 入力スキーマ（要点）

各 `strategies[]` は:
`name, source_tier, rank, symbol, timeframe_minutes, timeframe_label, direction,
indicators[[name,...params]], entry_ast, exit_ast, entry_pseudo, exit_pseudo,
sl, tp, volume, metrics{sharpe,profit_factor,win_rate,trades,max_dd,oos_sharpe,cpcv_*},
provenance{generation,parents,hash,immortal,...}, likely_test_artifact`

`entry_ast` / `exit_ast` は演算子前置のツリー（例:
`["AND",[">","CLOSE","EMA-20"],["CROSS-ABOVE","LOW","EMA-20"]]`）。
DSL演算子・指標の意味は `README.md` のグロッサリ参照。

## フェーズ0 — 取り込みとトリアージ

1. `edges_library.json` をロード。
2. 除外: `likely_test_artifact == true`、`provenance.hash` の重複（同一戦略が複数Tierに重複）。
3. 一次フィルタ: `metrics.trades >= 100`（疎サンプル除外）。ただし `source_tier == "LEGEND"`
   は残し「昇華候補」プールへ回す（少trade=即棄却にしない）。
4. 候補を3群に分類:
   - **A群（即検証）**: trades十分・指標がプラス側。
   - **B群（昇華対象）**: ロジックは妥当だが in-sample が弱い/負け。
   - **C群（廃棄）**: テスト成果物・崩壊（極端な負Sharpe×大trade）・重複。

## フェーズ1 — MQL5変換

`entry_ast` / `exit_ast` を再帰的にMQL5へ翻訳するジェネレータを書く:
- 比較 `>`,`<`,`>=`,`<=` → そのまま。
- `CROSS-ABOVE x y` → `prev(x) <= prev(y) && cur(x) > cur(y)`（`CROSS-BELOW`は逆）。
- 指標ref → `EMA-n→iMA(...,MODE_EMA)`, `SMA-n→iMA(...,MODE_SMA)`, `RSI→iRSI`,
  `MACD-LINE/SIGNAL-LINE→iMACD`, `BB-*→iBands`, `STOCH-K/D→iStochastic`,
  `ATR-14→iATR`, `CCI-14→iCCI`, `PSAR→iSAR`。
- `sl`/`tp` は価格単位（JPYペアは概ね pips×0.01）。`PNL/TP/SL` を使う育種exitは
  含み損益ベースのトレーリング/利確として実装。
- 1戦略=1 EA（またはパラメータ化した汎用EA＋セット）として出力。

## フェーズ2 — 厳格検証（落とすための検証）

各候補に対して:
1. **OOS分割**: 学習期間でパラメータ確定 → 触っていない期間で評価。
2. **ウォークフォワード**（purge/embargo付き、最低3フォールド）。
3. **コスト込み**でネット成績を算出。
4. **頑健性**: 少なくとも2シンボル / 隣接タイムフレームでも崩れないか。
5. **過学習診断**: Deflated/Probabilistic Sharpe、トレード順のMonte Carlo、
   パラメータ近傍の感度（高原か尖頂か）。

## フェーズ3 — 昇華（“天啓”：非エッジをエッジに引き上げる）

B群（および惜しくも落ちたA群）に対し、**仮説を持って**変換を適用し、再検証する。
闇雲なパラメータ弄りは禁止。各変換は「なぜ効くはず」を1行で言語化してから試す。

昇華テクニック（例）:
- **レジームフィルタ**: ADX/ATR でトレンド相場のみ/レンジ相場のみに限定。
- **セッション/時間帯フィルタ**: 東京・ロンドン・NY、指標時刻回避。
- **上位足コンフィルム**: 上位TFのトレンド方向と一致したときだけ発注。
- **ボラ連動SL/TP**: 固定pips → ATR倍数へ。
- **方向の非対称性**: long-only / short-only にして優位な側だけ残す。
- **反転**: 一貫して負の期待値 → シグナルを反転して検証（コスト後に正なら採用）。
- **アンサンブル/合議**: 相関の低い複数候補の合意で発注、単独の弱さを相殺。
- **コスト回避**: 高スプレッド時間帯のエントリを抑制。
- **エントリ/イグジット分離**: 良いエントリ × 別の優れたイグジットへ差し替え。

各昇華版は「親 `hash` + 適用した変換」を系譜として記録する。

## フェーズ4 — エッジ認定ゲート

**全条件をネット（コスト後）・OOSで満たしたものだけ**を edge と認定:
- OOS Profit Factor ≥ 1.30
- OOS Sharpe ≥ 0.50
- OOS MaxDD < 15%
- OOS トレード数 ≥ 100
- ウォークフォワード 3フォールド中 ≥ 2 で黒字、または ≥ 2シンボルで黒字
- パラメータ感度が高原（近傍で成績が崩れない）

満たさないものは `graveyard/` に理由付きで退避（昇華の学習データとして保持）。

## 成果物

1. `edges/` … 認定エッジの EA/セット + 1ページのエッジカード
   （ロジック / 想定レジーム / OOS成績 / コスト前提 / 失敗条件 / 系譜）。
2. `edges_ranked.csv` … 認定エッジを OOS Sharpe×PF×頑健性で順位付け。
3. `sublimation_log.md` … 各昇華の仮説・適用変換・結果（採否と理由）。
4. `graveyard/` … 落選候補と却下理由。

## 進め方

まず **フェーズ0のトリアージ結果（各群の件数と代表例）** を報告し、検証に進む
バッチ（例: LEGENDのトレンド系10本）を提案してから着手すること。
一度に全168本を回さず、仮説→検証→昇華のループを小さく回す。
