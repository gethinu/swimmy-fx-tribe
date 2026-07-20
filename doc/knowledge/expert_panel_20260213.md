# 🦅 Expert Panel Report (Critique)

**Date:** 2026-02-13  
**Leader:** Elon Musk  
**Mode:** critique  
**Trigger:** メモリ圧迫（`VmmemWSL` 約6.8GB、WSL内 `used 8.0GiB / 14GiB`, `swap 1.8GiB`）に対して「止めずに軽量化するにはどうするか？」

## 観測サマリ
- 主犯は常駐系。`swimmy-data-keeper` が約2.4GB、`swimmy-school` が約2.1GB、`guardian --backtest-only` が4 worker 合計で約1.4GB。
- `school` は 15万件級の重い処理を毎サイクル実行し、同時に prune が backlog 条件でスキップされ続ける状態。
- 設定ノブが分散しており、運用者が「効くと思って調整した値」が別コンポーネントには効かない箇所がある。

## 🏛️ 常設顧問の意見
### Taleb
「今の設計は“死なないように見えて、静かに破綻へ向かう”タイプだ。上限がないシステムはいつか上限に達する。」
- Data Keeper は大きな固定バッファを常時確保する設計（`tools/data_keeper.py:60`, `tools/data_keeper.py:61`）。
- さらに起動時に全TFを eager load しており、メモリ・起動時間を前払いで消費する（`tools/data_keeper.py:188`, `tools/data_keeper.py:193`）。
- `swimmy-data-keeper.service` は `StartLimitIntervalSec` を `[Service]` に置いており無効。実際に systemd が Unknown key を出している。安全装置が“あるつもり”になっている（`systemd/swimmy-data-keeper.service:13`）。
  > [!NOTE] **訂正 (2026-07-12 / P4, stale)**: この指摘は解消済。現行 `systemd/swimmy-data-keeper.service:4-5` は `StartLimitIntervalSec=300` / `StartLimitBurst=5` を正しく `[Unit]` に置いており実害なし。P4 で cgroup `MemoryMax=3G` も追加（Taleb の「上限がない」指摘に対応）。

### Graham
「複雑さの借金を、CPUとRAMで返済している。これはスケールしない。」
- `school` は毎サイクルで Wisdom を走らせる構造で、処理周期が短すぎる（`src/lisp/school/school-connector.lisp:219`, `src/lisp/school/school-connector.lisp:243`）。
- prune は incubator backlog があるとスキップされるため、重くなるほど軽量化が働かない（`src/lisp/school/school-connector.lisp:116`, `src/lisp/school/school-connector.lisp:120`）。
- ドキュメントは「6時間ごと」「週次」と書いているが実装は1時間。運用者が実態を誤解する（`doc/owners_guide.md:157`, `docs/llm/ARCHITECTURE.md:92`, `src/lisp/school/school-connector.lisp:109`）。

### Naval
「レバレッジは“正しいノブ”に接続されて初めて効く。今はノブが多すぎる。」
- Lisp送信側は `SWIMMY_BACKTEST_MAX_PENDING`、Python受信側は `SWIMMY_BACKTEST_MAX_INFLIGHT` を使う。名前が近く、運用で誤設定を誘発する（`src/lisp/core/config.lisp:117`, `tools/backtest_service.py:70`）。
- worker は `.env` で 8 に上げられており（`.env:59`）、`guardian` 常駐プロセス数を直接増やす（`tools/backtest_service.py:65`, `tools/backtest_service.py:750`）。

### Jim Simons
「統計系バッチを毎秒ループに埋め込むのは、アルゴリズム設計として非効率だ。」
- `analyze-veterans` が `remove-duplicates`→`filter`→`copy-list+sort` を毎サイクル実施。15万件規模ではメモリ一時確保が大きすぎる（`src/lisp/school/school-breeder.lisp:771`, `src/lisp/school/school-breeder.lisp:787`）。
- これは“学習頻度”と“市場反応頻度”を分離していないことが原因。反応ループと重い分析ループを同居させるべきではない。

## 💻 技術パネルの意見
### Fowler
「設定責務が分裂している。設定値が宣言されても、実行経路で使われない。」
- `swimmy-brain.service` は `SWIMMY_SBCL_DYNAMIC_SPACE_MB` を定義しているが（`systemd/swimmy-brain.service:17`）、実際の起動スクリプトは 4096 固定で無視している（`run.sh:64`）。
- “設定があるのに効かない”状態は運用事故を量産する。これは設計の失敗。
  > [!NOTE] **訂正 (2026-07-12 / P4, stale)**: 「4096 固定で無視」は誤り。`run.sh` は当時から env を尊重していた（現行 `run.sh:69` は `${SWIMMY_BRAIN_HEAP_MB:-${SWIMMY_SBCL_DYNAMIC_SPACE_MB:-6144}}`）。P4 でヒープを `/etc/swimmy/heap.env` に一元化し brain=3072 / school=4096、変数名を daemon 別に分離した。Fowler の「設定が実行経路で使われない」問題は解消（unit が heap.env を最後に読む単一正本）。

### Hickey
「Simple made easy を逆行している。巨大な可変構造をぐるぐる回している。」
- `*strategy-knowledge-base*` 全体に対する重い変換を毎サイクルで回しており、データ指向ではなくメモリ帯域依存になっている（`src/lisp/school/school-breeder.lisp:768`, `src/lisp/school/school-breeder.lisp:792`）。
- Data Keeper も dict-of-dict-of-deque で1件あたりオブジェクトコストが高い（`tools/data_keeper.py:67`, `tools/data_keeper.py:130`）。

### Uncle Bob
「この問題は“最適化不足”ではなく“運用要件のテスト不足”だ。」
- “ドキュメント記載値と実装値の一致”をCIで検証していない。M1 10M と書いて実装は 500k（`docs/llm/ARCHITECTURE.md:75`, `tools/data_keeper.py:60`）。
- systemd unit の妥当性（セクション配置/キー有効性）も自動検証がない（`systemd/swimmy-data-keeper.service:13`）。

## 🚀 ビジョナリーの意見
### Ng
「制御なしの高並列は学習ではなく発熱になる。」
- worker 増加（6→8）は STATE には記録されているが、メモリ予算連動の自動制御がない（`docs/llm/STATE.md:95`, `docs/llm/STATE.md:138`）。
- “性能改善”はスループット/遅延/メモリの3軸で制御ループ化すべき。

### López de Prado
「検証品質の前に、実行安定性を確保しろ。メモリ圧迫は検証結果そのものを歪める。」
- CPCV/OOSの評価系を回す土台が不安定だと、失敗率の解釈が運用ノイズに汚染される。
- まず backtest 実行層の負荷を予算化し、キュー制御と worker 数を一貫した1つの運用指標で管理すべき（`src/lisp/core/config.lisp:117`, `tools/backtest_service.py:70`）。

### Gene Kim
「これは典型的な“ドキュメント正本と実行正本の分離事故”だ。」
- `doc/SYSTEM_ARCHITECTURE.md` は deprecated で、`docs/llm/ARCHITECTURE.md` が正本だが、そこでも実装とのドリフトが残っている（`doc/SYSTEM_ARCHITECTURE.md:1`, `docs/llm/ARCHITECTURE.md:3`）。
- 運用ガード（メモリ上限、ユニット検証、設定整合チェック）を CI/監査に組み込むまで同じ事故は再発する。

## 🚀 Musk's Decision (Final)
> 「結論はシンプルだ。止めずに軽くするなら“順序”を守れ。  
> まずノブを減らし、次に重いループを間引き、最後にデータ構造を直す。  
> いきなり全面最適化はやるな。故障点が増えるだけだ。」

**やること（Do）**
1. Backtest worker を段階的に下げる（8→3→2）＋ `MAX_INFLIGHT` を明示し、メモリ/throughput を計測して最適点を決める。  
2. `analyze-veterans` を毎サイクルから時間間隔実行へ分離し、prune の skip 条件を見直す。  
3. Data Keeper を lazy-load 化し、M1/Tick の保持上限を env 化して動的調整可能にする。  
4. docs/llm 正本と実装値の整合チェックを CI へ追加する。

**やらないこと（Don’t）**
1. いきなり Data Keeper の全面書き換え（tuple化/列指向化）を先にやらない。まず運用ノブで圧を抜く。  
2. 1回のリリースで worker・school cadence・data構造を同時変更しない（原因帰属不能になる）。

## Actionable Items
1. `SWIMMY_BACKTEST_WORKERS` を 8→3 に下げ、`SWIMMY_BACKTEST_MAX_INFLIGHT` を新設して実測比較（`.env`, `tools/backtest_service.py:65`, `tools/backtest_service.py:70`）。
2. `phase-7-wisdom-update` を毎サイクル実行から間隔実行へ変更（`src/lisp/school/school-connector.lisp:219`）。
3. `phase-8-weekly-prune` の skip 条件（incubator backlog）を緩和し、hard-cap が効く状態を担保（`src/lisp/school/school-connector.lisp:116`, `src/lisp/school/school-pruning.lisp:30`）。
4. Data Keeper の `MAX_CANDLES_PER_SYMBOL`/`MAX_TICKS_PER_SYMBOL` を env 化し、起動時は M1 を lazy-load（`tools/data_keeper.py:60`, `tools/data_keeper.py:61`, `tools/data_keeper.py:193`）。
5. ~~`run.sh` の `--dynamic-space-size` を env 参照へ統一し、`swimmy-brain.service` と整合~~ **✅ 済 (P4)**: `run.sh` は元々 env 尊重。P4 で `/etc/swimmy/heap.env` 一元化（brain 3072 / school 4096、変数名分離）。→ `doc/knowledge/p4_reliability_ops_20260712.md`。
6. ~~`systemd/swimmy-data-keeper.service` の `StartLimit*` を `[Unit]` へ移して検証スクリプト追加~~ **✅ 済**: 現行は `[Unit]` に正しく配置。P4 で `MemoryMax=3G` 追加＋`tests/systemd_reliability_test.sh` で回帰防止。
7. docsの実値整合を修正（M1保持上限、prune頻度）: `docs/llm/ARCHITECTURE.md:75`, `docs/llm/STATE.md:71`, `doc/owners_guide.md:157`。
