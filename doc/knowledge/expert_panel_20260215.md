# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-15  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「8TFを常識にせず、H60(=3600分)やM36/M45/H2/H3.5/H7/H8.5など任意TFも“強ければ貪欲に採用”。ただしTFの無限増殖でデータ/カテゴリ/計算が破綻しない設計は？」  

**Purpose:** TFを内部表現「分(int)」に統一しつつ、任意TF探索を“金鉱掘り”として成立させる。H60戦略はバグ起源でも利益が出ているなら保持して使う。  
**Constraints:** 既存DBの`timeframe=300/3600`等を破壊しない。ライブ実行で「違うTFの足を見ていた」を絶対に起こさない。TF増加でDataKeeperのRAM/起動時間/ディスクが爆発しない。  
**Success Criteria:**  
1. `strategy-timeframe`は常に「分(int)」として扱える（文字列/秒の混入を吸収）。  
2. 任意TFでも backtest と execution が同じ足（同じ境界）で評価され、unknown TF のサイレントフォールバックが無い。  
3. TF探索はできるが、カテゴリやS枠がTF無限で無限増殖しない。  
4. 低頻度戦略（例: 35 trades / 10y）の“見かけのSharpe爆上げ”を抑制できる。  
**Assumptions:** M1の履歴が取得可能（ディスクCSV or MT5からbackfill）。任意TFはM1からのリサンプルで生成できる。  
**Uncertainties:** 月足は「固定分」ではなくカレンダーバーであり、`43200分`表現は便宜上の符号でしかない。MT5/Guardian/DataKeeperのどこでカレンダー境界を正本化するかで挙動が割れる。  

## 観測サマリ（まず現状は“TF探索以前に壊れている”）
- “秒を分だと思っている”seedが存在する。`legends.lisp`は `:timeframe 3600 ; H1` と書いており、設計が分(int)なら正しくは60であるべき（`src/lisp/strategies/legends.lisp:13`）。同様に `:timeframe 300 ; M5`（本来は5）もある（`src/lisp/strategies/legends.lisp:31`）。このズレが「H60(3600分)」「H5(300分)」の怪我の功名を生んでいる。  
- 進化・LLM生成はTFをハード固定している。変異TF候補は `(5 15 60 240)` のみ（`src/lisp/school/school-evolution.lisp:186-197`）。LLMプロンプトも「Timeframe MUST be one of 5/15/60/240」と明示固定（`src/lisp/school/school-evolution-llm.lisp:44-50`）。  
- ところが運用レポート上は `M3600`/`M300` が主要カテゴリになっている（`data/reports/evolution_factory_report.txt:52-58`）。つまり「探索は固定」なのに「現場は固定から逸脱したTFが主役」という矛盾状態。  
- カテゴリキーが生TF（分）をそのまま使うため、TFを自由化するとカテゴリ数が無限化する（`src/lisp/school/school-strategy.lisp:154-169`）。相関/クローン排除のスコープも `(timeframe × direction × symbol)` なので、TFを変えるだけでクローンがすり抜ける（`src/lisp/school/school-kb.lisp:175-190`）。  
- 実行経路に「未知TFはM1扱い」のサイレントフォールバックがある。`prepare-trade-context`は未知の`timeframe`を`timeframe-key`で`(t "M1")`に落とす（`src/lisp/school/school-execution.lisp:321-330`）。backtest側のTF文字列化も未知は`"M1"`（`src/lisp/school/school-backtest.lisp:160-174`）。これがある限り、任意TF探索は“勝って見えるだけの取り違え”を量産する。  
- DataKeeperは固定TF文字列しか受理しない（`VALID_TIMEFRAMES`）（`tools/data_keeper.py:64-110`）。しかも起動時ロード対象TFに`"MN"`を含めるが（`tools/data_keeper.py:263-270`）、ディスクには`*_MN1.csv`や`*_Monthly.csv`が存在し、命名が噛み合っていない（例: `data/historical/EURUSD_MN1.csv`）。読み込み候補も`{symbol}_{tf}.csv`固定（`tools/data_keeper.py:271-279`, `tools/data_keeper.py:412-420`）。  
- Sharpe算出が「ゼロリターン日を除外」しており、低頻度ほどSharpeが盛れる（`guardian/src/backtester.rs:229-246`）。H60戦略（35 trades）を“爆強”認定しやすい最悪の歪み。  
- MT5側のTF対応は狭い。`StringToTimeframe`はM1/M5/M15/M30/H1/H4/H12/D1/W1/MNのみ（`src/mt5/SwimmyBridge.mq5:343-355`）。H2/H3/H6/H8等ですら未対応なので、任意TFを本気でやるなら「MT5に頼らずM1から集約」が前提になる。  

## 🏛️ 常設顧問の意見
### Taleb（Ruin回避）
「“探索を増やせば儲かる”は嘘になりやすい。探索を増やすほど、偽物のエッジを拾って破滅確率が上がる。」
1. 選択肢A: TF候補を有限集合に固定（例: 8TF+追加数個）。  
Tradeoff: 実装は最速で安全だが、金鉱探索の上限が低い。  
2. 選択肢B: 任意TFは許容するが、カテゴリ/昇格/枠は「TFバケット」で上限化する。  
Tradeoff: 探索自由度と破滅防止の両立。ただし設計変更が必要（`school-strategy.lisp`/`school-kb.lisp`のキー設計）。  
3. 選択肢C: 無制限探索 + 厳格な統計補正（Deflated Sharpe, multiple testing）で抑え込む。  
Tradeoff: 正しさは上がるが、計算と実装が重く、運用で理解されにくい。  
地雷: 現状は“未知TFがM1に落ちる”ため、探索自体がロシアンルーレット（`src/lisp/school/school-execution.lisp:321-330`）。まずここを殺せ。  

### Graham（スケールしないこと）
「TF無限化はプロダクトのスケール問題。データ・カテゴリ・検証が指数的に膨らむ。」
1. 選択肢A: データはM1だけ正本化し、他TFはオンデマンド集約 + LRUキャッシュ。  
Tradeoff: ディスク爆発を止められるが、CPUと実装複雑性が増える（DataKeeper改修）。  
2. 選択肢B: 主要TFだけ事前生成・永続化（頻出TFのみ）し、その他は集約しない。  
Tradeoff: 実装は簡単だが、探索できるTFが限定される。  
3. 選択肢C: MT5/ブローカー依存のTF（MN1等）は「カレンダーTF」として別系統で扱う。  
Tradeoff: 正しいが、内部表現「分(int)」だけでは嘘になる（`tools/data_keeper.py:268-279`のMN読み込み不整合が既に兆候）。  
反証（あえて）: TFを増やす前に、評価歪み（Sharpeのゼロ除外）を直さないと探索は“儲け”ではなく“幻覚の量産”になる（`guardian/src/backtester.rs:229-246`）。  

### Naval（レバレッジ/自動化）
「AIの良さは貪欲さだが、貪欲さは“コストに接続されたノブ”でしか制御できない。」
1. 選択肢A: TF探索に“予算”を導入（1日あたりの新規TF数、backtest回数、CPU秒）。  
Tradeoff: 破綻しない。代わりに探索が鈍る。  
2. 選択肢B: Multi-armed banditでTFに予算配分（勝ってるTFほど回すが、一定確率で新規も掘る）。  
Tradeoff: 金鉱掘りっぽい。だが実装は少し重い。  
3. 選択肢C: “TFを変えるだけのクローン”を自動で潰す（バケット/正規化で同一カテゴリ扱い）。  
Tradeoff: 探索の自由が減るが、KB/墓場の膨張が止まる（`src/lisp/school/school-kb.lisp:175-190`が現状すり抜け点）。  

### Jim Simons（数学/パターン）
「H60が勝つなら採用で良い。ただし“勝った”の定義が壊れている。」
1. 選択肢A: TFを遺伝子（ハイパーパラメータ）として連続的に探索。ただし評価は“観測数”で正規化する。  
Tradeoff: 発見力が上がるが、評価の統計設計が必須。  
2. 選択肢B: TFを離散候補集合に落とし、探索は指標/ロジックに集中。  
Tradeoff: 速いが取り逃しが出る。  
3. 選択肢C: H60のような低頻度戦略はSharpeではなくPSR/DSRや期待値の信頼区間で昇格判定する。  
Tradeoff: 遅いが、破滅確率が下がる（現状Sharpeが盛れる設計は危険）。  

## 💻 技術パネルの意見
### Fowler（設計/責務）
「TFが“分/文字列/秒”で混在しているのは、責務が分裂している証拠。変換関数が散らばり、未知TFのデフォルトが複数ある。」
1. 選択肢A: TF正規化を単一モジュールに集約し、全経路がそれを使う（Lisp/Python/Rust/MT5）。  
Tradeoff: 直す箇所は多いが、以降の拡張が楽。  
2. 選択肢B: まずは“サイレントフォールバック禁止”だけ先に入れる（未知TFはエラー/テレメトリ）。  
Tradeoff: 早いが、機能は増えない。  
3. 選択肢C: DBに`timeframe_minutes`と`timeframe_code`（MN1等）を持たせ、表現を明確化する。  
Tradeoff: 正しいがマイグレーションが必要。  
根拠: 現状でもTF表現が各所で別（`src/lisp/school/school-backtest.lisp:160-193`, `src/lisp/school/school-execution.lisp:321-330`, `tools/backtest_service.py:581-596`）。  

### Hickey（Simple）
「“任意TFを許す”はSimpleじゃない。Simpleにするなら“正本は1つ”にしろ。」
1. 選択肢A: 正本データはM1のみ保持し、任意TFは関数（集約）で得る。  
Tradeoff: データ爆発を止められるが、キャッシュ設計が必要。  
2. 選択肢B: TF集合を増やしても、保持するのは“必要な分だけ”にする（active strategiesのTFのみ）。  
Tradeoff: ライブの動作は安定するが、バックテスト時に毎回計算が増える。  
3. 選択肢C: `*timeframes*`のような文字列集合を正本にしない。内部は分(int)だけにして、表示だけ変換する（`src/lisp/school/school-constants.lisp:8`）。  

### Uncle Bob（テスト）
「TF自由化は事故を呼ぶ。テストなしでやると“違うTFで取引”が本番で起きる。」
1. 選択肢A: 契約テストを先に作る。未知TFがM1に落ちないこと、同じTFでBT/Executionが一致することを固定。  
Tradeoff: 実装は遅れるが、後で死なない。  
2. 選択肢B: TF変換のユニットテストを追加（分→ラベル、ラベル→分、MN/MN1の扱い）。  
Tradeoff: 安い。  
3. 選択肢C: “H60キープ”を明示テストする（`3600`を`60`に矯正しない）。  
Tradeoff: バグの功名を守れるが、将来の開発者の誤修正を防ぐドキュメントが必要（`src/lisp/strategies/legends.lisp:13`が罠）。  

## 🚀 ビジョナリーの意見
### Ng（制御ループ）
「探索空間が増えるなら、制御ループが必要。今はTFが固定のつもりで作られている。」
1. 選択肢A: TF探索を“段階制”にする（Successive Halving/Hyperband的に、短期→長期で評価を深める）。  
Tradeoff: 計算資源を節約できるが、実装が増える。  
2. 選択肢B: TFごとに“期待スコア”の事後分布を持ち、探索と活用を自動配分する。  
Tradeoff: 良いが複雑。  
3. 選択肢C: TFを増やす前に、評価指標の欠陥（Sharpe盛り）を直して教師信号を正す。  
Tradeoff: 発見は遅れるが、学習は本物になる。  

### López de Prado（オーバーフィット）
「TF探索は典型的なmultiple testing。無限に掘れば“勝つTF”は必ず出る。だから“発見したら儲かる”ではない。」
1. 選択肢A: Deflated Sharpe/PSRを導入し、観測数（非ゼロ日/取引数）で補正する。  
Tradeoff: 本物に寄るが、門が狭くなる。  
2. 選択肢B: カテゴリをTFバケット化し、TFでの“すり抜け”を潰す（枠制限の意味を守る）。  
Tradeoff: 金鉱が減るが、破綻確率が減る（`src/lisp/school/school-strategy.lisp:154-169`が今の破綻点）。  
3. 選択肢C: H60など低頻度戦略は「時間あたり期待値」「コスト込み期待値」「OOSの安定性」で評価する。  
Tradeoff: Sharpe幻想が消える。代わりに地味な戦略が残る。  

### Gene Kim（運用）
「TF自由化は運用コストの自由化でもある。正本が揺れると障害解析が不可能になる。」
1. 選択肢A: テレメトリ/レポート表示を統一し、`M3600`を`H60`のように人間可読にする（内部は分でも良い）。  
Tradeoff: バグ調査が楽になる。  
2. 選択肢B: DataKeeperのディスク命名を正本化（MN/MN1/Monthlyの吸収）し、ロード漏れを潰す。  
Tradeoff: 事故が減る。  
3. 選択肢C: 任意TFは“サポート対象”と“実験対象”を分け、S-rank昇格に必要な運用条件（監視/緊急停止/検証）を厳格化する。  
Tradeoff: 利益機会は減るが、事故は減る。  

## 🚀 Musk's Decision (Final)
> 「結論はこうだ。H60は“勝つなら使う”。ただし今のままTFを増やすのはダメだ。  
> まず“TF正規化”と“サイレントフォールバック排除”をやれ。次に“データ爆発しない集約”を作れ。  
> その上で、TF探索は無制限ではなく“予算制”でやる。儲けのための探索が、破滅のための探索になったら終わりだ。」

**やること（Do）**
1. 内部TFは分(int)に統一し、表示は`M{n}`/`H{n}`/`H{n}.5`/`D{n}`/`W{n}`/`MN1`等へフォーマットする（巨大な43200はUIに出さない）。  
2. unknown TF の`M1`フォールバックを禁止し、エラー or 明示リサンプルに置換する（Execution/Backtestの両方）。  
3. DataKeeperは「M1正本 + 任意TFオンデマンド集約 + キャッシュ」に寄せ、ディスク命名（MN/MN1/Monthly）も吸収する。  
4. カテゴリ/相関スコープはTFバケット化して有限化し、TFを増やしてもS枠が無限にならないようにする。  
5. Sharpeのゼロ除外を止め、低頻度戦略を過大評価しない評価系にする。  

**やらないこと（Don’t）**
1. TF自由化だけを先に入れない（今は“違う足で評価して勝った気になる”地雷がある）。  
2. 任意TFを全て事前生成して永続化しない（DataKeeper/ディスクが死ぬ）。  
3. `timeframe=300/3600`を自動矯正しない（H60/H5が勝っているなら保持が前提）。  

## Actionable Items
1. TF正規化の正本化: `timeframe-minutes`/`timeframe-label`/`timeframe-bucket` を1箇所に実装し、Lispの変換分岐を削減: `src/lisp/school/school-backtest.lisp:160-193`, `src/lisp/school/school-execution.lisp:321-330`。  
2. サイレントフォールバック排除: unknown TF を`"M1"`に落とす分岐を撤去し、リサンプルかエラーへ: `src/lisp/school/school-backtest.lisp:160-174`, `src/lisp/school/school-execution.lisp:322-330`。  
3. DataKeeperを任意TF対応へ: `VALID_TIMEFRAMES`固定を廃止し、`tf_minutes(int)`入力を受けてM1から集約、MN/MN1/Monthlyの読み込み候補を追加: `tools/data_keeper.py:64-110`, `tools/data_keeper.py:263-279`, `tools/data_keeper.py:412-420`。  
4. カテゴリ有限化: `(TF×Dir×Symbol)`のTFをバケット化して枠制限と相関排除を守る: `src/lisp/school/school-strategy.lisp:154-169`, `src/lisp/school/school-kb.lisp:175-190`。  
5. 探索の予算制: 進化/LLMのTF固定を撤去しつつ、探索候補は予算で制御（bandit/段階評価）: `src/lisp/school/school-evolution.lisp:186-197`, `src/lisp/school/school-evolution-llm.lisp:44-50`。  
6. 評価歪み修正: Sharpeのゼロ除外を見直し、低頻度戦略にPSR/DSRや観測数ゲートを追加: `guardian/src/backtester.rs:229-246`。  
7. 月足の正本化: `"MN"`/`"MN1"`/`"Monthly"`を統一し、ディスクファイル命名とロードを一致させる（`*_MN1.csv`が存在するのに`MN`ロードしている矛盾を解消）: `tools/data_keeper.py:268-279`。  

---

# 🦅 Expert Panel Report (Consult) - Polymarket Weather Model (A/B/C)

**Date:** 2026-02-15  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** 「モデル強化（決定論の上位互換: multi-model/ensemble + 校正）に入る。A/B/Cどれがいい？個人的にはB」  

**Purpose:** Polymarket weather（最高気温レンジ）で、`p_yes` の精度とトレードEVの再現性を上げる。特に「外部予報の不確実性」と「マーケット価格」のギャップを、アンサンブルと校正で埋める。  
**Constraints:**  
- 15分スナップショット運用を崩さない（ランタイムが伸びても“終わる”こと、ハングしないこと）。`systemd/swimmy-polymarket-weather-snapshot.service` は oneshot で回る。  
- 取引側の入力は `OpenClawSignal(market_id,p_yes,confidence)` なので、最終出力はここに収める（`tools/polymarket_openclaw_bot.py:88-92`）。  
- 外部依存が増えるほど可用性が落ちるので、失敗時のフォールバックが必須。  
**Success Criteria:**  
1. 予測: logloss/brier が“将来データ”で改善（時系列split）。例: 既存 90日で `logloss=0.343553`（`data/reports/polymarket_openclaw/weather_backtest_runtime_2025-11-14_to_2026-02-14.json`）。  
2. 取引: 30dのPnLが“偶然”でなく再現（少なくとも複数30d窓で同方向）。現状 30d `+62.23` だが 28 trades と少ない（同JSON）。  
3. 運用: signal生成が遅くてもタイムアウトで失敗が記録され、スナップショットは完走する（`tools/polymarket_weather_snapshot.py:79-86` が現状ノーガード）。  
**Assumptions:** Open-Meteo / Gamma / CLOB のエンドポイントは安定稼働し、短時間の欠損は許容できる。校正学習に使う解決済み市場が継続的に溜まる。  
**Uncertainties:**  
- live信号は Open-Meteo forecast の“どのモデル”が既定なのか不透明で、backtest（`models=gfs_seamless`）とズレている可能性がある（`tools/weather_open_meteo_signal.py:408-453`, `tools/polymarket_weather_backtest.py:717-727`）。  
- 予報誤差は都市/季節/地形で変わるのに、現状 `sigma` は lead_days のみ（`tools/weather_open_meteo_signal.py:132-141`）。  

## 観測サマリ（現状の弱点は“校正欠如 + 予報モデル不一致 + 運用タイムアウト欠如”）
- 予測モデルは `mu=forecast_max` + `sigma_f_for_lead_days` の単純ガウス。校正なし（`tools/weather_open_meteo_signal.py:512-535`, `tools/weather_range_model.py:221-250`）。  
- 取引のエッジは `edge = p_yes - price` で直に効く。つまり校正のズレはEVのズレ（`tools/polymarket_openclaw_bot.py:498-541`）。  
- backtestは historical forecast に `models=gfs_seamless` を固定しているが、live信号はモデル指定が無い（`tools/polymarket_weather_backtest.py:717-727`, `tools/weather_open_meteo_signal.py:416-425`）。  
- signal sync は最終的に `OpenClawSignal` へ落として `market_id/p_yes/confidence` 以外を捨てる（`tools/openclaw_signal_sync.py:119-182`, `tools/polymarket_openclaw_bot.py:136-165`）。デバッグ/学習用の説明変数が消える。  

## 🏛️ 常設顧問の意見
### Taleb（Ruin回避）
「B（外部ソース追加）は“性能”より先に“脆さ”が来る。予測が良くても、タイムアウトで信号が欠けたら取引は死ぬ。」
1. 選択肢A: まず C（校正のみ）で“過信”を殺す。  
Tradeoff: 予報の平均（mu）の改善は無いが、EVの見積もりが現実に寄る。  
2. 選択肢B: Bはやるにしても “B-lite（同一API内の複数モデル） + 失敗時フォールバック” で脆性を上限化。  
Tradeoff: 改善余地は残しつつ、外部依存の爆増を避けられる。  
3. 選択肢C: 完全B（複数プロバイダ/NOAA直）をいきなりやる。  
Tradeoff: 分散できるが、運用事故の確率が上がり、スナップショットの“90日土台”が腐る。  
地雷: `tools/polymarket_weather_snapshot.py:83` の signal command は timeout無し。Bで最初に壊れるのは精度ではなく完走性。  

### Graham（スケールしないこと）
「Bは“賢いことをやってる感”が強すぎる。まず改善のボトルネックを特定しろ。」
1. 選択肢A: C（校正）で1週間だけやってログロスが動くか見る。  
Tradeoff: 速い。動けばBをやらなくていい。  
2. 選択肢B: B-lite（Open-Meteoの `models` 指定だけ）で backtest/live を揃える。  
Tradeoff: “モデル不一致”というデカいノイズ源を潰せる。  
3. 選択肢C: B（複数ソース）で本格エンジン化。  
Tradeoff: コードが“気象予報プラットフォーム”になり、Swimmyの本筋から逸れる。  

### Naval（レバレッジ/自動化）
「レバレッジは“自動で回る改善ループ”から生まれる。Bは運用コストでレバレッジを食う可能性がある。」
1. 選択肢A: 校正学習を毎日回し、`calibration.json` を更新して信号生成に反映（完全自動化）。  
Tradeoff: 外部依存を増やさず改善速度を出せる。  
2. 選択肢B: B-lite + 校正。モデルは2つまで、残りは“失敗したら捨てる”。  
Tradeoff: レバレッジを守りつつ多様性を取れる。  
3. 選択肢C: Bフル。  
Tradeoff: 研究が運用に負けて止まる。  

### Jim Simons（数学/パターン）
「“どれが良いか”は感覚で決めるな。評価設計で決めろ。Cだけで勝てるなら、Bは無駄。」
1. 選択肢A: C（校正）を時系列で学習し、翌月にだけ適用して測る。  
Tradeoff: ほぼゼロコストで“効く/効かない”がわかる。  
2. 選択肢B: A（sigma-mixture）で `p = Σ w_k * P(mu, sigma_k)` を作り、wを固定して校正。  
Tradeoff: 追加APIなしで“不確実性の形”を柔らかくできる。  
3. 選択肢C: B-lite（複数モデル）で `mu` 自体を複数化し、単純平均 + 校正。  
Tradeoff: 予報の平均が改善する可能性。ただし相関が高いと効果は薄い。  
指摘: 取引のロジックは `edge = p_yes - price` なので、校正は“勝率”というより“EV”を直撃する（`tools/polymarket_openclaw_bot.py:498-541`）。  

## 💻 技術パネルの意見
### Fowler（設計/責務）
「いまの構造だと B は “天気予報APIの例外処理地獄” を `weather_open_meteo_signal.py` に押し込むだけになる。」
1. 選択肢A: forecast取得を `fetch_forecast_open_meteo()` の単一関数から “provider/モデル” 抽象へ分離し、戻り値を共通型に統一。  
Tradeoff: 改修は増えるが、Bの追加が破綻しない。  
2. 選択肢B: まずは live/backtest のモデル指定を揃える（`models=gfs_seamless` 等）だけを先に入れる。  
Tradeoff: 設計は増えない。改善が出る可能性が高い。  
3. 選択肢C: Bフルで機能追加を先にやる。  
Tradeoff: 失敗した時に“どこが悪いか”が追えない。  
地雷: signal sync は `OpenClawSignal` に落として特徴量を捨てる（`tools/openclaw_signal_sync.py:176-182`）。Bをやるなら監査用に raw を残せ。  

### Hickey（Simple）
「Bをやるなら“Simpleなデータ変換”に落とせ。モデル分岐をif文で増やすな。」
1. 選択肢A: “予報（mu系列）→確率→校正” を純関数として合成し、I/Oは外側に閉じ込める。  
Tradeoff: テスト容易。運用事故時の切り分けが楽。  
2. 選択肢B: B-lite でも “モデル名の配列→結果の配列→平均” の形に固定する。  
Tradeoff: 追加のたびに壊れない。  
3. 選択肢C: そもそもBをやらず、A/Cで勝つ。  
Tradeoff: Simple。だが mu の系統誤差は残る。  

### Uncle Bob（テスト）
「Bは壊れる。壊れた時に“沈黙して勝ったふり”をするのが最悪だ。今の signal command はタイムアウト無しで沈黙し得る（`tools/polymarket_weather_snapshot.py:79-86`）。」
1. 選択肢A: まず失敗系テストを増やす（モデルが1つ落ちても残りで完走、全部落ちたら非ゼロ終了）。  
Tradeoff: 速度は落ちるが事故が減る。  
2. 選択肢B: 校正器（isotonic/Platt）をユニットテストで固定し、回帰を防ぐ。  
Tradeoff: 地味だが効く。  
3. 選択肢C: Bを“手動実験”として隔離し、本番パスに入れない。  
Tradeoff: 安全。だが自動化できない。  

## 🚀 ビジョナリーの意見
### Ng（AI/ML）
「Bに行くなら、必ず“校正 + 時系列評価”をセットにしろ。いまは校正が無い（`tools/weather_open_meteo_signal.py:521-535`）。」
1. 選択肢A: C（校正のみ）でベースラインを作る。  
Tradeoff: 一番早く、改善の有無が検証できる。  
2. 選択肢B: B-lite（複数モデル）で `p_raw` を作り、校正で `p_cal` に変換して `p_yes` として出す。  
Tradeoff: 期待値は高いが、データ/評価設計が必要。  
3. 選択肢C: A（sigma-mixture）+ 校正。  
Tradeoff: モデルを増やさずに表現力を増やせる。  

### López de Prado（オーバーフィット）
「Bは“モデル追加 = 多重検定”。良かったモデルだけ残すと、必ず将来で崩れる。」
1. 選択肢A: Bをやるならモデル数を固定し、重み学習は禁止（まず平均）。  
Tradeoff: 改善は小さいが過学習しにくい。  
2. 選択肢B: 校正も時系列splitでやれ（未来を見た校正は犯罪）。  
Tradeoff: 進捗は遅いが本物になる。  
3. 選択肢C: 30dのPnLだけで判断するな。28 trades は統計的に弱い（同JSON）。  
Tradeoff: 感情は満たされないが破滅しない。  
反証（対抗意見）: 「でもBで mu が良くなれば勝てる」はあり得る。ただし“勝ったか”の判定は厳格に。  

### Gene Kim（運用）
「Bは“運用のバグ”になる。遅い、落ちる、レート制限される。だから“可観測性”が先。」
1. 選択肢A: まずタイムアウトとログ（各モデルの成功/失敗/時間）を入れる。  
Tradeoff: 性能は上がらないが、壊れた時に直せる。  
2. 選択肢B: signal sync をraw保存対応にして、後追い検証を可能にする（`tools/openclaw_signal_sync.py:176-182` が今は捨てている）。  
Tradeoff: ディスクは増えるが1TBあるなら安い。  
3. 選択肢C: Bフルをやるなら“落ちた時にCへ自動降格”を仕込め。  
Tradeoff: 品質は下がるが稼働は続く。  

## 🚀 Musk's Decision (Final)
> 「Bでいい。ただし“Bフル”じゃない。B-lite + 校正を最短で入れろ。  
> いまの最大の無駄は、backtestとliveで予報モデルが違う可能性と、校正が無いことだ。  
> まずそこを潰して、改善が頭打ちなら外部ソース追加を考えろ。」

**やること（Do）**
1. B-lite: 同一プロバイダ（Open-Meteo）内の複数モデルを2-3個に限定し、単純平均（またはtrimmed mean）で `mu` を合成。  
2. C（校正）を必須化: `p_raw` を `p_cal` に変換し、取引は `p_cal` を `p_yes` として使う（`tools/polymarket_openclaw_bot.py:498-541` の入力を正す）。  
3. live/backtest のモデル指定を揃える（少なくとも“モデル名”を明示してズレをなくす）。  
4. 運用ガード: signal command にtimeoutを入れて、スナップショットが完走するようにする（`tools/polymarket_weather_snapshot.py:79-86`）。  

**やらないこと（Don’t）**
1. Bフル（複数外部プロバイダ/NOAA直）を先にやらない。運用で死ぬ。  
2. モデル重み学習（stacking）をいきなり入れない。まず平均 + 校正で十分。  
3. 30d PnL “だけ”でモデルを選ばない。精度指標と時系列評価を正本にする。  

## Actionable Items
1. live予報のモデル指定を導入し、backtestと揃える: `tools/weather_open_meteo_signal.py:408-453`, `tools/polymarket_weather_backtest.py:717-727`。  
2. B-lite実装: `tools/weather_open_meteo_signal.py` に `--forecast-model`（複数指定）を追加し、失敗時フォールバック（残りモデルで継続）を実装。  
3. 校正器の学習ツールを追加（isotonic/Platt）。入力は解決済み市場 + 予測値、出力は `data/models/polymarket_weather/calibration.json` のようなアーティファクト。適用は `tools/weather_open_meteo_signal.py:521-535` の `p_yes` 算出後。  
4. backtestにも同じ校正器を適用できるようにする（評価の正本化）: `tools/polymarket_weather_backtest.py:386-407`。  
5. snapshot collector の signal 実行に timeout と stderr 記録を入れ、`errors.jsonl` に残す: `tools/polymarket_weather_snapshot.py:79-86`。  
6. （任意だが推奨）signal sync をraw保存対応にして、学習/監査を可能にする: `tools/openclaw_signal_sync.py:119-182`。  
