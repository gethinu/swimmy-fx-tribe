# System Audit Automation Design (2026-02-03)

## 1. 目的・スコープ・安全策
目的は、監査の実行から必要な自動修復までを一発で完了し、運用の属人性を下げること。正本スコープは **systemd(system)** とし、`sudo systemctl` を使用する。対象サービスは `swimmy-brain/guardian/school/data-keeper/notifier` を既定とし、`SWIMMY_AUDIT_SERVICES` で追加指定できるようにする。  

監査範囲は以下を必須とする:
- サービス状態 (systemd)
- dashboard
- Discord通知経路
- Evolution report
- データ整合性 (integrity/deep audit)

自動修復は **daemon-reload + enable/disable整合 + restart** に限定し、DB操作やファイル削除などの破壊的操作は行わない。修復対象の判断は、必須サービスが inactive/failed/disabled の場合や、unit警告が出ている場合。  

`deep_audit` は SBCL ヒープ不足を回避するため **`--dynamic-space-size 4096`** を標準化する。結果は summary と next actions を必ず出力し、CI/cron でも利用可能な exit code を返す。  

## 2. 実装構成・実行フロー
`tools/system_audit.sh` を新設し、`tools/sbcl_env.sh` を読み込んで `SWIMMY_SBCL_DYNAMIC_SPACE_MB` を標準化する。監査は各ステップの成否を記録し、最後にまとめて判定する。  

1. **Service health**: `sudo systemctl status` で必須サービスを確認  
2. **Auto repair**: `sudo systemctl daemon-reload` → 必須サービス `enable` を保証 → `restart`  
3. **Dashboard**: `python3 tools/dashboard.py`  
4. **Notifier**: `tail -n 200 logs/notifier.log` + `python3 tools/test_notifier_direct.py`  
5. **Discord routing**: `sbcl --dynamic-space-size 4096 --script tools/broadcast_test_v2.lisp`  
6. **Reports**: `data/reports/evolution_factory_report.txt` の末尾確認  
7. **Data integrity**: `sbcl --dynamic-space-size 4096 --script tools/integrity_audit.lisp` + `tools/deep_audit.lisp`  

`DRY_RUN=1` で修復をスキップし、監査のみ実行可能にする。  

## 3. エラー処理・安全性・テスト
破壊的操作は行わず、修復は systemd の reload/enable/restart に限定。`enable/disable` は **必須サービスの enable 保証のみ**を行い、不要サービスの disable は行わない。  

エラー処理は「集計して最後に判定」。FAIL が1件でもあれば exit 1、WARN のみなら exit 2、すべてOKなら exit 0 を返す。ログは `logs/system_audit.log` に追記し、標準出力には summary と next actions を表示する。  

テストは軽量に以下を確認:
- `DRY_RUN=1 tools/system_audit.sh` で summary が出る
- `--help` が動作する
- `sbcl`/`python3` 未導入時に WARN を出して継続する
