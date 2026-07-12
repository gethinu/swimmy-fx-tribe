# 🛡️ P4 信頼性（クラッシュループ／メモリ／ヒープ）— 実装到達点と deploy 手順

**作成日:** 2026-07-12 JST
**スレッド:** B（P4/P5）／`rebuild_design_fable_P1-P7_20260703.md` §3 P4
**ステータス:** 実装済（オフライン・段階コミット）。deploy はオーナーの WSL(Ubuntu, user=swimmy) 上で実施。
**制約:** ライブ発注なし・オフライン検証のみ・gate floor 不変。

> [!IMPORTANT]
> 本 P4 の変更は **systemd ユニット／shell／env の設定のみ**。Rust Guardian の
> ライブ発注・清算セマンティクスは**一切変更していない**（下記 §4 の理由）。

---

## 1. クラッシュループ是正（毒データ無限再起動の根治）

| ユニット | Before | After |
|---|---|---|
| swimmy-brain | `Restart=always` / 10s / **StartLimit 無し** | `Restart=on-failure` / **30s** / `StartLimitIntervalSec=600` + `StartLimitBurst=5` / `OnFailure=swimmy-alert@%n.service` |
| swimmy-school | `Restart=always` / 10s / **StartLimit 無し** | 同上 |

- school-daemon は「Let It Crash」（`src/lisp/school/school-daemon.lisp:10-17` が未捕捉例外で `exit 1`）。`always`＋無制限では毒データを無限再起動で不死化していた。
- **600秒内に5回失敗すると unit は `failed` に入り恒久停止**。`OnFailure` が alert を1回だけ発火。
- restore_legend_61 の毎起動 ExecStartPre は **P2c で stamp 方式化済**（`tools/restore_legend_61.lisp`：source 署名一致時は full load をスキップし exit 0）。P4 での追加変更は不要。

## 2. OnFailure alert（恒久停止の可視化）

- `systemd/swimmy-alert@.service`（oneshot テンプレート）＋ `tools/ops/alert_on_failure.sh`。
- 失敗した unit 名を `%i` で受け取り、**durable ログ（logs/alert.log）を必ず記録**。`SWIMMY_DISCORD_ALERTS` が設定済のときのみ Discord 通知（best-effort）。
- alerter 自身は `Restart=`／`OnFailure=` を持たず、常に `exit 0` ＝ **fail-loop しない**。

## 3. ヒープ／メモリ配分

- **`/etc/swimmy/heap.env` に一元化**（`config/heap.env` が repo 正本）。両 unit が `EnvironmentFile=-/etc/swimmy/heap.env` を**最後に**読み、単一の正本として in-unit 既定・`.env` を上書き。
- **変数名を daemon 別に分離**：`SWIMMY_BRAIN_HEAP_MB=3072` / `SWIMMY_SCHOOL_HEAP_MB=4096`。旧 `SWIMMY_SBCL_DYNAMIC_SPACE_MB` の一括置換事故を防止。
- `run.sh`（brain）は `SWIMMY_BRAIN_HEAP_MB` → 旧共有変数 → 6144（bare dev 既定）の順で解決。native Windows の `scripts/windows/run.ps1` も同順で parity。
- 単発ツール（`tools/*.sh`、`finalize_rank_report`、`system_audit`）は従来どおり `SWIMMY_SBCL_DYNAMIC_SPACE_MB` を使用（**意図的に非改変**）。
- **swimmy-data-keeper に `MemoryMax=3G`**（cgroup）。暴走 read/merge が brain/school の heap を枯渇させるのを防ぐ。OOM 時は当該 unit のみ kill→`Restart=always` が再起動。

## 4. Guardian degraded 運転（brain 恒久停止時）— 既存実装で成立

**結論：P4a の StartLimit で brain が恒久停止すると、Guardian の既存 brain-disconnect ゲートが degraded 運転を成立させる。Rust の追加変更は不要。**

連鎖：

1. brain が 600s 内 5回失敗 → systemd が `failed`（恒久停止）。
2. `OnFailure` → alert 1回。
3. Guardian は brain 無音を検知（`should_trigger_brain_timeout` `guardian/src/main.rs:882`、既定 timeout 300s `:1008`）し `brain_connected=false` `:1170` にする。
4. **新規エントリ全停止**：risk gate `check_order` が `if !self.brain_connected { return Err(VetoReason::BrainTimeout) }` `guardian/src/main.rs:802-803` で BUY/SELL を veto。
5. **既存ポジは管理継続**：Guardian の主ループは市場データ処理・PnL 追跡・週末クローズ・SL/TP 管理を継続（新規のみ停止）。
6. **revival loop 防止**：Guardian の auto-revival は 300s 内 2 death で halt `guardian/src/main.rs:1106-1107,1208`。brain 側 StartLimit(5/600s) と両立（Guardian が先に諦める）。`failed` 状態の brain への `systemctl restart` は systemd が rate-limit で拒否 → 恒久停止が保たれる。

### ⚠️ オーナー判断事項（本 P4 では未変更）
brain タイムアウト時、現行の dead-man switch は短TF ポジを **emergency close**（`CLOSE_SHORT_TF`＋`CANCEL_ALL` `guardian/src/main.rs:1359-1373`、D1+ は保護）する。
- 設計の「既存ポジ**保護**のみ」を「短TFは清算して縮小・D1+のみ保持」と読むなら**現行のままで整合**。
- 「全ポジを保持して SL/TP 管理のみ」に変えるなら **ライブ清算セマンティクスの変更**＝ hard-to-reverse・要オフライン外検証のため、**オーナーの明示判断が必要**。本 P4 では現行挙動を保持した。

---

## 5. Deploy 手順（WSL Ubuntu / user=swimmy）

```bash
# 0. リポジトリ最新化（このブランチを WSL 側へ）
cd /home/swimmy/swimmy

# 1. ヒープ正本を設置
sudo tools/ops/install_heap_env.sh
#    -> /etc/swimmy/heap.env (BRAIN=3072 / SCHOOL=4096)

# 2. unit を配置（system 正本）
sudo cp systemd/swimmy-brain.service systemd/swimmy-school.service \
        systemd/swimmy-data-keeper.service systemd/swimmy-alert@.service \
        /etc/systemd/system/
sudo systemctl daemon-reload

# 3. 再起動
sudo systemctl restart swimmy-brain swimmy-school swimmy-data-keeper

# 4. 検証
systemctl show swimmy-brain  -p Restart,RestartUSec,StartLimitIntervalUSec,StartLimitBurst
systemctl show swimmy-school -p Restart,RestartUSec,StartLimitIntervalUSec,StartLimitBurst
systemctl cat  swimmy-brain | grep -E 'BRAIN_HEAP|heap.env|OnFailure'
systemctl show swimmy-data-keeper -p MemoryMax
#    OnFailure の疎通は手動テスト:
#    sudo systemctl start swimmy-alert@swimmy-brain.service ; tail logs/alert.log
```

期待値：`Restart=on-failure` / `RestartUSec=30s` / `StartLimitIntervalUSec=10min` / `StartLimitBurst=5`、brain heap=3072・school heap=4096、data-keeper `MemoryMax=3221225472`(3G)。

---

## 6. 回帰防止

- `tests/systemd_reliability_test.sh`：school/brain の on-failure＋StartLimit＋OnFailure、heap.env の変数分離、data-keeper MemoryMax、alerter が fail-loop 防止（Restart 無し）を assert。
- `.gitattributes`：`*.sh`/`*.service`/`*.timer`/`*.env` を `eol=lf` にピン（CR 混入で shebang/unit ディレクティブが壊れるのを防止）。
