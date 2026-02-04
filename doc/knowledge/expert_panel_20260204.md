# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-04
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** "BACKTEST V2の`timeframe`表現がテストとズレている。INTERFACES優先かテスト優先か、どう決めるべきか？"
**Purpose:** Backtest V2の`Option`表現を仕様・実装・テストで一致させ、Guardian互換と進化パイプラインの信頼性を守る。
**Constraints:** 内部ZMQはS式のみ、Backtest Serviceはserde_lexprの`Option<T>`表現（空/1要素リスト）を前提（`docs/llm/INTERFACES.md#L193`）。
**Success Criteria:** `test-backtest-v2-uses-alist`が仕様に沿ってパスし、Backtest Service/Guardian側のパース互換を壊さない。
**Assumptions:** `timeframe`はBacktest V2で`Option<i64>`扱いである（`docs/llm/INTERFACES.md#L217`）。
**Uncertainties:** Guardian側の受信実装が`(timeframe . 1)`と`(timeframe 1)`の両方を受理するかは未検証。

## 🏛️ 常設顧問の意見
### Taleb:
- **批判**: テストが仕様より「偶然の内部表現」に依存している。仕様は`Option`を「空/1要素リスト」と明記しているのに、テストはドットペア前提で壊れている（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) **仕様優先**: 実装は`(timeframe 1)`のまま、テストを`(cadr ...)`/`(first (cdr ...))`に修正。
  2) **互換拡張**: 受信側で`(timeframe . 1)`も`(timeframe 1)`も許容（破壊的変更の回避）。
  3) **仕様破り**: `timeframe`をドットペアに統一（ただしOption規約違反）。
- **トレードオフ**: 1が最小リスク。2は防御的だが複雑化。3は将来の破綻リスク。
- **具体根拠**: `request-backtest-v2`は`(list 'timeframe timeframe)`でリスト化している（`src/lisp/school/school-backtest-v2.lisp#L58`）。
- **反証**: Guardianが「ドットペアのみ受理」なら仕様が嘘になる。だが仕様を変えるなら`INTERFACES`を修正してからやれ。

### Graham:
- **批判**: テストは「契約」じゃなく「実装詳細」を検証している。仕様の整合性より、テストの都合が勝っているのは最悪（`src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) **最小変更**: テストだけ修正し、仕様と実装を一致させる。
  2) **仕様を変える**: `INTERFACES`を更新してテストに合わせる（ただし全依存箇所へ波及）。
  3) **抽象化**: `Option`を生成するヘルパーを導入し、テストはヘルパーに追従。
- **トレードオフ**: 1が最短。2は全域修正が必要。3は将来の負債削減だが今は余計。
- **具体根拠**: `INTERFACES`に明記済みの`Option`表現（`docs/llm/INTERFACES.md#L193`）。

### Naval:
- **批判**: 仕様とテストの二重管理が「レバレッジ」を殺している。単一の真実（INTERFACES）から機械的にテストを生成するべき。
- **選択肢**:
  1) 仕様優先 + テスト修正。
  2) 仕様優先 + `Option`整合のユーティリティを作る。
  3) テスト優先だが、その場合は仕様更新を強制。
- **トレードオフ**: 2は長期的に強いが短期工数。
- **具体根拠**: `timeframe`の表現はBacktest V2 payloadで明示的にリスト化されている（`src/lisp/school/school-backtest-v2.lisp#L58`）。

### Simons:
- **批判**: 数値フィールドの型契約が曖昧なままテストに反映されている。`Option<i64>`を表す構文を「一貫」させよ。
- **選択肢**:
  1) `Option`表現を仕様通りに維持し、テストを合わせる。
  2) 受信側で2種類の表現を許可して、送信側は仕様通りのまま。
  3) `Option`ヘルパーを導入して全送信を正規化。
- **トレードオフ**: 3が最も統計的に安全だが、今は小さな不整合の修正が先。
- **具体根拠**: `Option`規約の明記（`docs/llm/INTERFACES.md#L193`）。

## 💻 技術パネルの意見
### Fowler:
- **批判**: `Option`のシリアライズ規約が「仕様」と「実装」「テスト」で分散している。`timeframe`がlistなのは仕様通りだが、テストが契約を破っている（`src/lisp/tests.lisp#L742`）。
- **選択肢**:
  1) 仕様優先でテストを修正。
  2) 仕様を更新し、テストも実装も同時に合わせる（大きい）。
  3) `Option`構築/検証のヘルパーを導入し、テストはそれを使う。
- **トレードオフ**: 1が最小。3は長期の技術負債に効く。
- **具体根拠**: `request-backtest-v2`のpayload構築（`src/lisp/school/school-backtest-v2.lisp#L51`）。

### Hickey:
- **批判**: データが複雑化しているのに、テストが「表現」に縛られている。S式はデータだ。表現の揺れを許容して本質を検証せよ。
- **選択肢**:
  1) `Option`はlistで統一し、テストは`(cadr ...)`で検証。
  2) parser側で両方許容。
- **トレードオフ**: 2は設計が重くなる。1で十分。
- **具体根拠**: `Option`仕様に基づくpayload（`docs/llm/INTERFACES.md#L193`, `src/lisp/school/school-backtest-v2.lisp#L58`）。

### Uncle Bob:
- **批判**: テストが間違った期待をしているのは、設計の「契約」がコードに反映されていないから。テストは契約を検証しろ。
- **選択肢**:
  1) テスト修正（`(numberp (cadr ...))`）で仕様に整合。
  2) 実装変更＋仕様更新のセットで対応。
- **トレードオフ**: 1が安全。2は変更範囲が広すぎる。
- **具体根拠**: テスト側の`(cdr (assoc 'timeframe ...))`がリストを返す（`src/lisp/tests.lisp#L742`）。

## 🚀 ビジョナリーの意見
### Ng:
- **批判**: Backtest結果が止まると進化が止まる。テストの誤仕様でパイプラインの可観測性が下がるのは致命的。
- **選択肢**:
  1) 仕様に合わせてテストを直す。
  2) 受信側の柔軟性も追加して「取りこぼし」を減らす。
- **トレードオフ**: 2は安全だが実装コストと複雑性が増える。
- **具体根拠**: Backtest V2 payloadとOption規約の不一致が原因（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

### López de Prado:
- **批判**: 進化パイプラインのデータ品質が落ちると、統計的に意味のない進化になる。仕様とテストのズレは「ノイズ源」だ。
- **選択肢**:
  1) 仕様通りにテストを修正。
  2) 送受信の両方でOptionのスキーマ検証を追加。
- **トレードオフ**: 2は本質的に良いが、まずは1でノイズ源を消せ。
- **具体根拠**: Option規約は明示されているのにテストが破っている（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

### Gene Kim:
- **批判**: テストが赤いままでは運用判断ができない。仕様ドリフトは運用リスク。
- **選択肢**:
  1) 仕様優先でテストを修正し、STATEに明記。
  2) 実装を変えるなら、影響範囲を洗ってから。
- **トレードオフ**: 1が即効性。2はリードタイムが伸びる。
- **具体根拠**: `docs/llm/INTERFACES.md`の`Option`表現とテストの不一致（`docs/llm/INTERFACES.md#L193`, `src/lisp/tests.lisp#L742`）。

## 🚀 Musk's Decision (Final)
> 「仕様が正義。`Option`表現は`(timeframe 1)`のまま維持し、テストを仕様に合わせる。受信側の柔軟化は“必要になったら”でいい。まず動くパイプラインを取り戻せ。」

## Actionable Items
1. `test-backtest-v2-uses-alist`を仕様準拠に修正（`(numberp (cadr ...))`等）: `src/lisp/tests.lisp#L742`
2. `INTERFACES`の`Option`記述が実装と一致していることを再確認・必要なら追記: `docs/llm/INTERFACES.md#L193`
3. `STATE`に「Backtest V2の`Option`表現はリスト形式が正本」と明記: `docs/llm/STATE.md`

---

# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-04
**Leader:** Elon Musk
**Mode:** consult
**Trigger:** "brain.lisp はブートローダーだが必要か？ run.sh / systemd と整合する最適な判断は？"
**Purpose:** swimmy-brain の起動経路（run.sh → brain.lisp）を最小の運用リスクで安定化し、将来のエントリポイント設計を判断する。
**Constraints:** systemd の ExecStart が `/home/swimmy/swimmy/run.sh` に固定（`/etc/systemd/system/swimmy-brain.service:10-12`）、run.sh が `brain.lisp` を直接ロード（`run.sh:28-29`）。停止時間は最小化、既存運用（owners_guide の再起動手順）を壊さない（`doc/owners_guide.md:201-211`）。
**Success Criteria:** swimmy-brain が安定起動し、`brain.lisp` 不在で停止しない。運用ドキュメントと実行系の不整合が解消される。
**Assumptions:** `brain.lisp` は ASDF ローダーで `swimmy.main:start-system` を起動するだけのブートローダー（`brain.lisp:1-32`）。
**Uncertainties:** ほかのスクリプト/運用が `brain.lisp` を直接参照しているか、systemd/運用上の「正本」が run.sh か別ランナーかは未検証（`doc/SYSTEM_ARCHITECTURE.md` の記述は V3.0 時点）。

## 🏛️ 常設顧問の意見
### Taleb:
- **批判**: ブートローダーの有無が単一点障害になっている。`brain.lisp` が無いだけで brain が停止するのは脆弱（`run.sh:28-29`）。
- **選択肢**:
  1) **現状維持**: `brain.lisp` を必須とし、欠落時は即フェイル＋アラート（run.sh で存在チェック）。
  2) **二重化**: run.sh が `brain.lisp` を優先、無ければ直接 ASDF 実行にフォールバック。
  3) **撤廃**: systemd の ExecStart を ASDF 直起動に変更し、`brain.lisp` を廃止。
- **トレードオフ**: 1は最短だが脆弱性固定。2は複雑化するが耐障害性が上がる。3は整理できるが運用変更が重い。
- **反証**: `brain.lisp` を残すだけならリスクは小さい。だが「存在しないと死ぬ」設計を放置すると再発する。
- **根拠**: systemd は run.sh を固定参照（`/etc/systemd/system/swimmy-brain.service:10-12`）。

### Graham:
- **批判**: エントリポイントの「真実」が曖昧。run.sh / brain.lisp / doc がバラバラで、デバッグコストを増やす（`doc/SYSTEM_ARCHITECTURE.md:41-82`）。
- **選択肢**:
  1) **現状維持**: brain.lisp を残し、doc を揃える。
  2) **run.sh を正本化**: run.sh のコメントと owners_guide に明記。
  3) **ASDF直起動**: run.sh を薄い wrapper にして brain.lisp を消す。
- **トレードオフ**: 1/2は即効性、3は構造改善だが移行コスト。

### Naval:
- **批判**: 手作業依存が残る。ブートローダーを「人が復元する」運用はレバレッジが低い。
- **選択肢**:
  1) **自動復元**: run.sh が `brain.lisp` を検出し、バックアップから復元。
  2) **単一エントリ**: systemd の ExecStart を固定スクリプト（`swim` 等）に統一。
  3) **廃止**: brain.lisp を消し ASDF 直起動。
- **トレードオフ**: 1は安全だが自動化の責務が増える。2/3は整理だが運用変更が必要。

### Simons:
- **批判**: 実行経路が曖昧だと「再現性」がない。運用はモデルと同じで決定論が必要。
- **選択肢**:
  1) run.sh + brain.lisp 固定化。
  2) systemd 直起動一本化。
- **トレードオフ**: 1は現状維持、2は整理。どちらにせよ「一本化」を徹底せよ。

## 💻 技術パネルの意見
### Fowler:
- **批判**: run.sh が brain.lisp に直結しており、構成がコードに埋め込まれている（`run.sh:28-29`）。
- **選択肢**:
  1) **最小修正**: brain.lisp を必須化し、存在チェックを追加。
  2) **構造改善**: run.sh を ASDF 直起動に切替。
  3) **段階移行**: 両方動く期間を設けてから廃止。
- **トレードオフ**: 3が安全だが手順が増える。

### Hickey:
- **批判**: 「ブートローダーがあるから安心」というのは幻想。重要なのは一貫性。
- **選択肢**:
  1) brain.lisp を「明示的な正本」にして docs で固定。
  2) ASDF 直起動に統一。
- **トレードオフ**: どちらか一つに寄せるだけで混乱は減る。

### Uncle Bob:
- **批判**: エントリポイントが曖昧なのは設計の不備。run.sh の責務が曖昧（`run.sh:1-29`）。
- **選択肢**:
  1) run.sh を「エントリポイント」だと明記し、brain.lisp を必須にする。
  2) run.sh を削って systemd が直接 ASDF を起動。
- **トレードオフ**: 1は短期、2はクリーン。

## 🚀 ビジョナリーの意見
### Ng:
- **批判**: brain が落ちるとバックテスト結果が集まらず進化が止まる。ブートストラップの不備は運用停止と同義。
- **選択肢**:
  1) brain.lisp を残し、存在チェック＋監視を追加。
  2) ASDF 直起動に切替えて単一点を消す。
- **トレードオフ**: 1は即効性、2は将来性。

### López de Prado:
- **批判**: 運用の不安定性はデータ欠損に直結する。起動経路は「統計的に安定」させるべき。
- **選択肢**:
  1) エントリを1本化。
  2) 2経路にして冗長化。
- **トレードオフ**: 2は冗長性があるが、状態不一致のリスクも増える。

### Gene Kim:
- **批判**: runbook に手順はあるが、実体が壊れていると再起動は無意味（`doc/owners_guide.md:201-211`）。
- **選択肢**:
  1) brain.lisp の存在を前提として明文化し、欠落時の復旧手順を追加。
  2) systemd を ASDF 直起動に変更して運用手順を簡素化。
- **トレードオフ**: 1は即修正、2は再設計。

## 🚀 Musk's Decision (Final)
> 「今は止血が最優先。**brain.lisp を正本として残す**。ただし“無いと死ぬ”を放置するな。run.sh に存在チェック＋復旧導線を入れ、並行して ASDF 直起動への移行計画を作れ。」

## Actionable Items
1. run.sh に `brain.lisp` の存在チェックと明確なエラーメッセージを追加（最低限のフェイルファスト）: `run.sh:28-29`
2. systemd の ExecStart と実行手順を owners_guide に一致させる（現状の実体を正本として明記）: `/etc/systemd/system/swimmy-brain.service:10-12`, `doc/owners_guide.md:201-211`
3. 中期で `brain.lisp` 廃止 or 維持の結論を出し、doc/SYSTEM_ARCHITECTURE.md の brain.lisp 参照を更新する: `doc/SYSTEM_ARCHITECTURE.md:41-82`

---

# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-04  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** "バックテストDiscord通知でRRバッチが1000/98636。処理をもっと加速できるか？どこまで行ける？"
**Purpose:** Phase 1 BT (RR) のスループットを上げ、フルサイクル時間を短縮する。  
**Constraints:** 送信は `SWIMMY_BACKTEST_MAX_PENDING` と `SWIMMY_BACKTEST_RATE_LIMIT` で抑制され、ZMQ送信自体に固定 sleep が入る（`src/lisp/school/school-backtest-utils.lisp:36-55`）。RRバッチは `max-batch-size 1000` がコード固定（`src/lisp/strategies/strategies.lisp:82-93`）。`request-backtest` 側は送信可否を返さず、RR側は送信失敗でも `requested-count` を増やすため、Discord表示が実送信とズレうる（`src/lisp/strategies/strategies.lisp:141-160`, `src/lisp/school/school-backtest.lisp:283-285`）。  
**Success Criteria:** フルRRサイクル時間を実測で短縮しつつ、`pending` が上限を超えない・Guardian/BacktestのEOF/ドロップ無し・DiscordのRequestedが実際の送信と整合する。  
**Assumptions:** Backtest Serviceが稼働中で、`swimmy.main::*backtest-recv-count*` が正しく更新されている（`src/lisp/school/school-backtest-utils.lisp:30-34`）。CSVファイルモードで稼働しており、I/Oが極端に詰まっていない（`src/lisp/school/school-backtest.lisp:238-255`）。  
**Uncertainties:** Backtest Serviceの実処理能力、ZMQ HWMの設定、ディスクI/Oのボトルネック、`requested-count`と実送信の乖離量。

## 🏛️ 常設顧問の意見
### Taleb:
- **批判**: 「速くする」前に、**何が落ちるか**が見えていない。今のRRは**送信失敗でもRequestedに加算**される（`src/lisp/strategies/strategies.lisp:141-160`）ので、数字が“成功”を偽装する。`send-zmq-msg`は**pending超過で送信しない**（`src/lisp/school/school-backtest-utils.lisp:36-55`）。このまま上げると**ルイン（詰まり/ドロップ）**を招く。
- **選択肢**:
  1) **安全増速**: `RATE_LIMIT=10 / MAX_PENDING=1000 / batch=2000`で様子見。  
  2) **適応制御**: pendingが閾値を超えたら自動でレートを落とす（`backtest-send-allowed-p`の閾値を動的化）。  
  3) **強行**: `RATE_LIMIT=50 / MAX_PENDING=8000 / batch=10000`。ただし監視なしは自殺。  
- **トレードオフ**: 1は安全だが時間がかかる。2は開発が必要。3は早いが失敗時の破壊が大きい。
- **反証/リスク**: Backtest Serviceが実は軽量で余裕があるなら、1は遅すぎる。だが**計測無しの高速化は賭博**だ。

### Graham:
- **批判**: `max-batch-size`が**ハードコード**されており、運用のレバーが無い（`src/lisp/strategies/strategies.lisp:82-93`）。「運用で調整する」設計思想と矛盾している。
- **選択肢**:
  1) **設定化**: `SWIMMY_BT_MAX_BATCH` を導入し、運用で変更可能にする。  
  2) **現状維持**: まずは env だけ上げる（`src/lisp/core/config.lisp:116-119`）。  
  3) **大改修**: RRを非同期キューに流し、バッチ概念を廃止。  
- **トレードオフ**: 1は実装軽い。2は効果が限定的。3は最大効果だが時間がかかる。

### Naval:
- **批判**: スループット改善は「運用で触れるレバー」が命。今は env 以外の可変レバーがなく、**学習ループにレバレッジが無い**。
- **選択肢**:
  1) **運用レバー増設**: batch sizeもenv化、結果を`backtest_status`に可視化。  
  2) **データ削減**: CSV overrideを軽量化しI/Oの飽和を避ける（`src/lisp/school/school-backtest.lisp:238-255`）。  
  3) **強制高速化**: `RATE_LIMIT`を大幅に上げ、Backtest側で詰まったら切り戻す。  
- **トレードオフ**: 1が長期的に最強。2は短期効果だが精度低下リスク。3はスピードは出るが事故率も上がる。

### Simons:
- **批判**: 「速さ」は統計品質を犠牲にしがち。**送信ドロップや欠測が増えると評価が歪む**。`backtest-pending-count`は `recv` が正確でないと破綻する（`src/lisp/school/school-backtest-utils.lisp:30-44`）。
- **選択肢**:
  1) **段階加速**: 10→25→50と段階的に上げ、pending/recvの比率が安定する範囲だけ許容。  
  2) **キャッシュ活用**: 24h以内のキャッシュなら**再送しない**設計に変える（`src/lisp/school/school-backtest-utils.lisp:115-123` を活かす）。  
  3) **強制高速化**: 1サイクル完走だけを優先して欠測は無視。  
- **トレードオフ**: 2は最も合理的だが設計変更。3は統計的に無意味なデータを増やす危険。

## 💻 技術パネルの意見
### Fowler:
- **批判**: `batch-backtest-knowledge` が**送信成功/失敗を考慮しない**（`src/lisp/strategies/strategies.lisp:141-160`）。観測指標が曖昧だと「速くなったか」の判断が不可能。
- **選択肢**:
  1) **メトリクス修正**: 送信成功時のみ `requested-count` を加算。  
  2) **閾値調整のみ**: envだけ上げて様子を見る。  
  3) **アーキ変更**: キュー化して実送信数を常にトラッキング。  
- **トレードオフ**: 1が最小改修で効果的。2は測定が歪む。3は最大効果だが重い。

### Hickey:
- **批判**: シンプルに言うと「**バッチを大きくしても送信は増えない**」。送信は `backtest-send-allowed-p` で止められる（`src/lisp/school/school-backtest-utils.lisp:36-44`）。その結果、余計なループが増えるだけ。
- **選択肢**:
  1) **事前チェック**: `send-zmq-msg`前に許可判定し、無駄なpayload作成を避ける。  
  2) **レート調整優先**: batchサイズは最小変更、rate/pendingでスループットを制御。  
  3) **バッチ拡大のみ**: ループだけ増える可能性が高い。  
- **トレードオフ**: 1はCPU削減になる。2は直感的で安全。3は見た目だけの加速。

### Uncle Bob:
- **批判**: 「加速したい」が仕様化されていない。テストも無い。`SWIMMY_BACKTEST_RATE_LIMIT` や `MAX_PENDING` の境界挙動は**自動テスト不在**（`src/lisp/core/config.lisp:116-119`）。
- **選択肢**:
  1) **境界テスト追加**: pending超過時の挙動をテストで固定。  
  2) **運用で調整**: まずは値を上げて実測。
  3) **大改修**: rate/pendingを自動調整にしてテスト設計。  
- **トレードオフ**: 1は品質向上だが工数。2は早いが事故要因。3は強いが重い。

## 🚀 ビジョナリーの意見
### Ng:
- **批判**: 速度は価値だが、**学習パイプラインの可観測性が無い**と意味がない。`requested`が実送信とズレるなら進化評価もズレる（`src/lisp/strategies/strategies.lisp:141-160`）。
- **選択肢**:
  1) **メトリクス整備 + 段階加速**。  
  2) **強加速**: 50/8000で突っ込み、落ちたら戻す。  
  3) **データ削減**: CSV軽量化で学習速度を底上げ。  
- **トレードオフ**: 1は安全だが遅い。2は早いが学習が不安定。3は精度劣化リスク。

### López de Prado:
- **批判**: 高速化は**サンプルの品質**を壊す。もしドロップが起きれば統計的に歪む。`backtest-pending-count`が崩れると、送信制御も無意味（`src/lisp/school/school-backtest-utils.lisp:30-44`）。
- **選択肢**:
  1) **安全増速**: 10/1000/2000。  
  2) **適応制御**: pending比率でレートを自動制御。  
  3) **強行**: 50/8000/10000は短期成果狙い。  
- **トレードオフ**: 2が最も合理的だが実装必要。1は地味だが最も信頼性が高い。3は統計的破綻の危険。

### Gene Kim:
- **批判**: 運用は「ロールバック可能」が前提。いきなり強め設定は事故リスク。`SWIMMY_BACKTEST_RATE_LIMIT`/`MAX_PENDING`は**systemdの再起動で変わる**ため、切替手順が必要（`src/lisp/core/config.lisp:116-119`, `doc/owners_guide.md:154-200`）。
- **選択肢**:
  1) **段階的変更**: Safe→Balance→Aggressiveの順で、各段階を1サイクル観測。  
  2) **一発強行**: 強めにしてログが荒れたら戻す。  
  3) **運用自動化**: systemd unitに環境変数セットを明記し、切替をコード化。  
- **トレードオフ**: 1が最も安全。2は短期だが事故率大。3は手間だが再現性が高い。

## 🚀 Musk's Decision (Final)
> 「**バランスまでなら現実的**。ただし“測れてないなら上げるな”。`RATE_LIMIT=25 / MAX_PENDING=3000 / batch=5000`は**上限ではなく“試験値”**。pendingが増え続けるなら即ロールバック。`requested`の嘘表示は直せ。強め設定は“計測が整ってから”だ。」

## Actionable Items
1. `batch-backtest-knowledge` の `requested-count` を送信成功時のみ加算する（`src/lisp/strategies/strategies.lisp:141-160`, `src/lisp/school/school-backtest.lisp:283-285`）。
2. `max-batch-size` を環境変数化（例: `SWIMMY_BT_MAX_BATCH`）し、運用で変更可能にする（`src/lisp/strategies/strategies.lisp:82-93`）。
3. 加速は **段階的に**：Safe→Balance→Aggressiveの順でpending/recvの比率が崩れないことを確認（`src/lisp/school/school-backtest-utils.lisp:30-44`）。

---

# 🦅 Expert Panel Report (Consult)

**Date:** 2026-02-04  
**Leader:** Elon Musk  
**Mode:** consult  
**Trigger:** "バックテストを加速したい。コード修正(②)で対応しつつ、動的変更(ランタイム反映)も可能にできるか？"  
**Purpose:** Phase 1 BT (RR) のスループットを上げつつ、`RATE_LIMIT/MAX_PENDING/BATCH` を運用中に安全に変更できるようにする。  
**Constraints:** 送信は `SWIMMY_BACKTEST_MAX_PENDING` と `SWIMMY_BACKTEST_RATE_LIMIT` で抑制され、`send-zmq-msg` に固定スリープがある（`src/lisp/school/school-backtest-utils.lisp:36-55`）。RRバッチは `max-batch-size 1000` がコード固定で、運用からは変更不可（`src/lisp/strategies/strategies.lisp:82-93`）。`request-backtest` は送信成否を返さず、RR側は送信失敗でも `requested-count` を増やすため観測が歪む（`src/lisp/strategies/strategies.lisp:141-160`, `src/lisp/school/school-backtest.lisp:283-285`）。  
**Success Criteria:** ランタイムで値を変更してもクラッシュせず、`pending` が上限を超えない・Backtest Service/Guardian側の詰まりが発生しない・DiscordのRequestedが実送信数と整合する。  
**Assumptions:** `backtest-pending-count` の `recv` が正しく更新されている（`src/lisp/school/school-backtest-utils.lisp:30-34`）。Hot Reload手順は運用可能（`doc/owners_guide.md:72-75`）。  
**Uncertainties:** Backtest Serviceの実効処理能力、ZMQ HWM設定、`send-zmq-msg` の固定スリープがボトルネックになるか、動的変更が他の送信経路（OOS/WFV等）に与える影響。

## 🏛️ 常設顧問の意見
### Taleb:
- **批判**: 「動的変更」は**事故の入口**。送信失敗でもRequestedが増える状態（`src/lisp/strategies/strategies.lisp:141-160`）での動的変更は、失敗の可視化をさらに悪化させる。
- **選択肢**:
  1) **保守的動的変更**: ランタイム変更は許可するが、上限は安全領域（例: rate<=25, pending<=3000）。  
  2) **段階昇圧**: 変更はステップ式のみ。`pending` が安定している時にだけ次段へ。  
  3) **動的変更禁止**: env変更＋reloadのみに限定。  
- **トレードオフ**: 1は柔軟だが事故時の説明責任が増える。2は安全だが実装が必要。3は安全だが運用の柔軟性が無い。
- **反証/リスク**: Backtest側が十分強ければ3は過剰防衛になる。だが計測が無いまま動的化は破滅寄り。

### Graham:
- **批判**: `max-batch-size` がローカル定数で運用レバーが存在しない（`src/lisp/strategies/strategies.lisp:82-93`）。この設計のまま動的化は「半分だけ動く」設計。
- **選択肢**:
  1) **env化**: `SWIMMY_BT_MAX_BATCH` を追加し、reloadで変更できるようにする。  
  2) **ランタイムAPI**: ZMQコマンドで `*rr-max-batch-size*` を更新する。  
  3) **現状維持**: rate/pendingのみ動的、batchは固定。  
- **トレードオフ**: 1は簡単だが即時反映ではない。2は柔軟だが設計とテストが必要。3は運用者が混乱する。

### Naval:
- **批判**: 運用のレバレッジ不足。動的変更を入れるなら**可観測性が前提**。
- **選択肢**:
  1) **ZMQコマンド**で即時反映 + 変更履歴をメトリクスに残す。  
  2) **設定ファイル監視**で自動反映（オペミス減）。  
  3) **reload運用**に限定してレバレッジを諦める。  
- **トレードオフ**: 1は実装最小だがアクセス制御が必要。2は便利だがバグ混入リスク。3は最も安全だが柔軟性が低い。

### Simons:
- **批判**: `send-zmq-msg` に固定 `sleep 0.005` があるため、**理論上200req/sで頭打ち**（`src/lisp/school/school-backtest-utils.lisp:49-55`）。レートを上げても意味が薄い。
- **選択肢**:
  1) **動的変更はrate/pending中心**にし、sleepの見直しは別タスク化。  
  2) **sleepも動的化**して真のスループットを解放。  
  3) **計測重視**: 送信遅延を測り、最適値を統計的に決める。  
- **トレードオフ**: 1は現実的。2は速いがGuardian側が詰まる可能性。3は確実だが遅い。

## 💻 技術パネルの意見
### Fowler:
- **批判**: 送信成否が反映されないまま動的化すると、運用が“嘘の指標”で判断する（`src/lisp/strategies/strategies.lisp:141-160`）。
- **選択肢**:
  1) **requested-count修正**: 送信成功時のみ加算。  
  2) **動的化は後回し**: まず観測修正。  
  3) **キュー化**: 実送信数と待ち行列を分離。  
- **トレードオフ**: 1が最小改修で効果大。2は安全だが遅い。3は正しいが重い。

### Hickey:
- **批判**: シンプルさを壊すな。動的変更は“最小の面積”でやるべき。  
- **選択肢**:
  1) **単純関数化**: `set-backtest-throttle` のような単一関数で更新し、globalsに集約。  
  2) **分散更新**: 複数箇所でsetf（バグ温床）。  
  3) **env reloadのみ**: シンプルだが即時性なし。  
- **トレードオフ**: 1が最適。2は破綻。3は安全だが遅い。

### Uncle Bob:
- **批判**: “動的変更”はテスト無しでは危険。`backtest-send-allowed-p` の境界挙動が保証されていない（`src/lisp/school/school-backtest-utils.lisp:36-44`）。
- **選択肢**:
  1) **境界テスト追加**してから動的化。  
  2) **運用で試す**（ただしロールバック手順必須）。  
  3) **動的化はやめる**。  
- **トレードオフ**: 1は安全だが時間。2は早いが事故率。3は安全だが柔軟性ゼロ。

## 🚀 ビジョナリーの意見
### Ng:
- **批判**: 学習パイプラインは“止まらない”ことが最優先。動的変更で詰まるなら意味がない。
- **選択肢**:
  1) **段階導入**: まずはenv+reloadで安定、次にランタイム化。  
  2) **即時ランタイム化**: ただし緊急停止コマンド必須。  
  3) **動的化しない**: 速度は犠牲。  
- **トレードオフ**: 1が最も現実的。2は速いが危険。3は保守的だが学習が遅い。

### López de Prado:
- **批判**: 速度よりもデータ品質。動的変更が“欠測”を生んだら統計が歪む。
- **選択肢**:
  1) **動的化はpendingが安定している時だけ許可**（ガード条件）。  
  2) **動的化の履歴をログ化**して評価を補正。  
  3) **固定運用**にする。  
- **トレードオフ**: 1と2の組み合わせが現実的。3は安全だが遅い。

### Gene Kim:
- **批判**: 運用は再現性が命。動的変更は**手順書とロールバック**がセットでないと事故る。
- **選択肢**:
  1) **ZMQコマンド + auditログ**: 変更はすべて記録。  
  2) **設定ファイル監視**: 変更履歴が残るが監視の堅牢性が必要。  
  3) **reload運用**: 既存の `./tools/reload.sh` を前提にする（`doc/owners_guide.md:72-75`）。  
- **トレードオフ**: 1は最も実用的。2は便利だが運用複雑。3は安全だが即時性に欠ける。

## 🚀 Musk's Decision (Final)
> 「**やるなら“ランタイム変更”まで行け**。ただし、`requested`の嘘表示と固定batchのまま動的化は許さない。先に**計測を正し、batchを可変化**してから、ZMQコマンドで即時更新できるようにしろ。ファイル監視は後回し。」

## Actionable Items
1. `requested-count` を送信成功時のみ加算する（`src/lisp/strategies/strategies.lisp:141-160`, `src/lisp/school/school-backtest.lisp:283-285`）。
2. `max-batch-size` を `SWIMMY_BT_MAX_BATCH` などで環境変数化し、グローバルに保持する（`src/lisp/strategies/strategies.lisp:82-93`）。
3. ランタイム変更用のZMQコマンドを追加し、`*backtest-max-pending* / *backtest-rate-limit-per-sec* / *rr-max-batch-size*` を単一関数で更新。変更履歴をログ化（`src/lisp/school/school-backtest-utils.lisp:36-44`, `src/lisp/core/globals.lisp:109-113`）。
