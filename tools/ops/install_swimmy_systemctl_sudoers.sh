#!/usr/bin/env bash
set -euo pipefail

src="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/swimmy-systemctl.sudoers"
dest="/etc/sudoers.d/swimmy-systemctl"
tmp="${dest}.tmp.$$"

if [[ "${EUID:-$(id -u)}" -ne 0 ]]; then
  echo "ERROR: must run as root." >&2
  echo "Run: sudo bash tools/ops/install_swimmy_systemctl_sudoers.sh" >&2
  exit 1
fi

if [[ ! -f "$src" ]]; then
  echo "ERROR: missing template: $src" >&2
  exit 1
fi

umask 027

cp "$src" "$tmp"
chown root:root "$tmp"
chmod 0440 "$tmp"

echo "[CHECK] validating sudoers syntax: $tmp"
/usr/sbin/visudo -cf "$tmp"

mv -f "$tmp" "$dest"

echo "[OK] installed: $dest"
echo "[NEXT] verify (as swimmy): sudo -n /usr/bin/systemctl status swimmy-brain.service"

