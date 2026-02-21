# Rank Conformance Audit Quickstart (2026-02-20)

## 1) 単発実行

```bash
python3 tools/check_rank_conformance.py \
  --db data/memory/swimmy.db \
  --out data/reports/rank_conformance_latest.json \
  --history-dir data/reports/rank_conformance
```

## 2) systemd（system scope）

```bash
sudo bash tools/install_rank_conformance_audit_service.sh
sudo systemctl start swimmy-rank-conformance-audit.service
sudo systemctl status --no-pager swimmy-rank-conformance-audit.timer
```

## 3) systemd（user scope, sudo不要）

```bash
mkdir -p ~/.config/systemd/user
install -m 0644 systemd/swimmy-rank-conformance-audit.user.service ~/.config/systemd/user/swimmy-rank-conformance-audit.service
install -m 0644 systemd/swimmy-rank-conformance-audit.timer ~/.config/systemd/user/
systemctl --user daemon-reload
systemctl --user enable --now swimmy-rank-conformance-audit.timer
```

## 4) しきい値設定

- `RANK_CONF_STRICT=1` のときだけ `RANK_CONF_MAX_VIOLATIONS` 超過で non-zero 終了。
- 既定 (`RANK_CONF_STRICT=0`) は違反件数を出力しつつ終了コードは 0。
