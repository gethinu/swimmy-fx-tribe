# 🗄️ P5 永続化の破損耐性 — 実装到達点と deploy 手順

**作成日:** 2026-07-12 JST
**スレッド:** B（P4/P5）／`rebuild_design_fable_P1-P7_20260703.md` §3 P5
**ステータス:** 実装済（オフライン・段階コミット）。deploy はオーナーの WSL(Ubuntu, user=swimmy) 上で実施。
**制約:** ライブ発注なし・オフライン検証のみ・gate floor 不変。
**関連:** [`p4_reliability_ops_20260712.md`](p4_reliability_ops_20260712.md)

---

## 1. 実装マップ（設計 P5 との対応）

| # | 設計項目 | 実装 |
|---|---|---|
| 1 | WAL / synchronous=NORMAL / busy_timeout | **既存で成立**（`src/lisp/core/sqlite-manager.lisp:32-34`、`get-db-connection` で毎接続設定）。P5 で追加変更なし。 |
| 2 | graveyard を BEGIN IMMEDIATE…COMMIT | `with-immediate-transaction`（sqlite-manager.lisp）を新設し、`record-graveyard-pattern`（school-cemetery.lisp）の rank flip + upsert を包む。RESERVED ロックを先取り＝ brain/school 競合での SQLITE_BUSY 敗北を排除。 |
| 3 | data_sexp に sha256 checksum ＋サイズ上限 | `sha256-hex`（ironclad, UTF-8）＋列 `data_sexp_sha256`（migration）。`upsert-strategy` が checksum 保存。`*max-data-sexp-length*`=256KB 超過は `:OVERSIZE:<len>` sentinel に差替（metric 列は保持、poison blob を DB に入れない）。 |
| 4 | tmp 名一意化（pid+epoch） | `save-strategy`（persistence.lisp）の temp-path を `<name>.<pid-proctoken>-<counter>.tmp` に。並行 writer の tmp 衝突を排除。 |
| 5 | 欠損検知監査（read-only 日次） | `tools/persistence_audit.py`（read-only）。graveyard 単調非減少 invariant（高水位 stamp）、checksum 整合、hash INVALID 比率、:CORRUPT 数、oversize 数、library ファイル数 vs SQL 行数突合。`swimmy-persistence-audit.service`/`.timer`（日次）。違反時 exit 1 → `OnFailure` alert。 |
| 6 | 破損行は削除せず quarantine | `cemetery-audit-db` が safe-read 不能行を `hash=':INVALID', rank=':CORRUPT'` に（削除しない）。`:CORRUPT` は active/breeding allow-list（B/A/S/LEGEND）外で自然除外、blob も `%deserialize-strategy-row` で drop。`%parse-rank-safe` は `:CORRUPT` を安全に parse。 |

## 2. 設計の「未確認」への回答

- **temp-path 同一FS（rename アトミック性の前提）**: ✅ **解決**。`save-strategy` の temp-path は `(merge-pathnames "<name>.<token>.tmp" path)` で、`path`（＝ `data/library/<rank>/<name>.lisp`）の**同一ディレクトリ**に生成される。同一 FS のため `rename-file` はアトミック。

## 3. 破損検知の設計思想

safe-read（`safe-read-sexp`）は破損を握り潰して nil を返す＝**静かな消失**が P5 の根本課題。対策は「握り潰しをやめる」ではなく「**別レイヤで気付く**」:
- 書込時に **checksum を刻む**（sha256）。
- 監査が **checksum 不整合・graveyard 減少・INVALID 比率**を read-only で検出。
- 破損行は **削除せず :CORRUPT へ隔離**（フォレンジック保全＋ active から除外）。
- graveyard の**単調非減少**を高水位 stamp で強制（append-only の失敗記録が縮んだら＝行喪失の確たる兆候→ HARD 違反）。

## 4. 検証（オフライン）

native-Windows は cl-sqlite の DLL 未ロードで full-system ロード不可のため、検証を分離:
- `sha256-hex`：SBCL+ironclad で NIST `"abc"` ベクタ一致・unicode 耐性を確認。
- SQL セマンティクス：`tests/p5_persistence_sql_test.py`（同一 SQLite エンジン, Python）で migration 冪等・BEGIN IMMEDIATE commit/rollback・checksum roundtrip/mismatch・oversize sentinel・:CORRUPT 除外/非削除・単調 invariant を検証（13/13 pass）。
- 監査 end-to-end：`tests/p5_audit_test.py`（合成 DB）で全 metric＋graveyard 行削除→ invariant 違反(exit 1) を検証。
- Lisp 構造健全性：編集5ファイルの paren-balance 確認。
- ⏳ **WSL ホストで要実行**：full `ql:quickload :swimmy` コンパイル通過確認（DLL がある Linux 側でのみ可能）。deploy 前に `sbcl --script` で `:swimmy` ロード→ `run-persistence-audit` 相当のスモークを推奨。

## 5. Deploy 手順（WSL Ubuntu / user=swimmy）

```bash
cd /home/swimmy/swimmy
# 1. 監査 timer/service を配置
sudo cp systemd/swimmy-persistence-audit.service systemd/swimmy-persistence-audit.timer \
        systemd/swimmy-alert@.service /etc/systemd/system/
sudo systemctl daemon-reload
sudo systemctl enable --now swimmy-persistence-audit.timer

# 2. 既存 DB のスキーマ migration は school 側の init-db が起動時に冪等適用
#    （ALTER TABLE ADD COLUMN data_sexp_sha256）。強制する場合は school 再起動。
sudo systemctl restart swimmy-school

# 3. 監査を手動実行（read-only）
.venv/bin/python3 tools/persistence_audit.py
#    -> logs/persistence_audit.log に JSON、graveyard 高水位 stamp を
#       data/memory/.graveyard_hwm に作成。

# 4. cemetery-audit-db を一度回して既存破損行を :CORRUPT へ隔離（school 内から）
#    （通常運転で hash IS NULL 行を検出時に自動適用）
```

**注意**: 初回監査は既存 DB の checksum 未設定行を `checksum_unchecked` として報告する（違反ではない）。upsert で再保存されるにつれ 0 に収束する。`data_sexp_sha256` が既存行に無い間は checksum 検査はスキップされる（列が無ければ検査自体を飛ばす）。
