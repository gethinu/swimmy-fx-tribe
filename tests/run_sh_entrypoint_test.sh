#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp_dir="$(mktemp -d)"
cleanup() {
  rm -rf "$tmp_dir"
}
trap cleanup EXIT

cp "$repo_root/run.sh" "$tmp_dir/run.sh"
chmod +x "$tmp_dir/run.sh"

marker="$tmp_dir/sbcl_marker.txt"
content_marker="${marker}.content"
cat > "$tmp_dir/sbcl" <<'FAKE'
#!/usr/bin/env bash
set -euo pipefail
load=""
for ((i=1;i<=$#;i++)); do
  if [ "${!i}" = "--load" ]; then
    j=$((i+1))
    load="${!j}"
    break
  fi
done
echo "$load" > "${SBCL_MARKER:?}"
if [ -f "$load" ]; then
  cat "$load" > "${SBCL_MARKER}.content"
fi
exit 0
FAKE
chmod +x "$tmp_dir/sbcl"

PATH="$tmp_dir:$PATH" SWIMMY_HOME="$tmp_dir" SBCL_MARKER="$marker" bash "$tmp_dir/run.sh" >/dev/null

if [ ! -s "$marker" ]; then
  echo "Expected sbcl marker to be written" >&2
  exit 1
fi

load_path="$(cat "$marker")"
if [ -z "$load_path" ]; then
  echo "Expected load path to be recorded by fake sbcl" >&2
  exit 1
fi

if [ ! -s "$content_marker" ]; then
  echo "Expected fake sbcl to capture boot file contents" >&2
  exit 1
fi

if ! grep -q "asdf:load-system" "$content_marker"; then
  echo "Expected fallback boot file to load ASDF" >&2
  exit 1
fi
