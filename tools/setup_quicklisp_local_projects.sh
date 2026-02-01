#!/usr/bin/env bash
set -euo pipefail

SWIMMY_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
QL_HOME="${QL_HOME:-$HOME/quicklisp}"
LOCAL_DIR="${QL_LOCAL_PROJECTS_DIRECTORY:-$QL_HOME/local-projects}"
ARCHIVES_DIR="$QL_HOME/dists/quicklisp/archives"
PATCH_DIR="$SWIMMY_HOME/tools/quicklisp_patches"

FORCE=0
if [[ "${1:-}" == "--force" ]]; then
  FORCE=1
fi

mkdir -p "$LOCAL_DIR"

ensure_project() {
  local name="$1"
  local archive="$2"
  local patch_file="$3"
  local dest="$LOCAL_DIR/$name"

  if [[ $FORCE -eq 1 && -d "$dest" ]]; then
    rm -rf "$dest"
  fi

  if [[ ! -d "$dest" ]]; then
    if [[ ! -f "$ARCHIVES_DIR/$archive" ]]; then
      echo "[ERROR] Quicklisp archive not found: $ARCHIVES_DIR/$archive" >&2
      exit 1
    fi
    tar -xzf "$ARCHIVES_DIR/$archive" -C "$LOCAL_DIR"
  fi

  if [[ ! -f "$PATCH_DIR/$patch_file" ]]; then
    echo "[ERROR] Patch not found: $PATCH_DIR/$patch_file" >&2
    exit 1
  fi

  patch -p1 -N -d "$dest" < "$PATCH_DIR/$patch_file"
}

echo "[QL] Local projects dir: $LOCAL_DIR"

ensure_project "pzmq-20210531-git" "pzmq-20210531-git.tgz" "pzmq-20210531-git.patch"
ensure_project "cl-sqlite-20190813-git" "cl-sqlite-20190813-git.tgz" "cl-sqlite-20190813-git.patch"
ensure_project "ironclad-v0.61" "ironclad-v0.61.tgz" "ironclad-v0.61.patch"
ensure_project "postmodern-20260101-git" "postmodern-20260101-git.tgz" "postmodern-20260101-git.patch"

echo "[QL] Registering local projects..."
SBCL_SETUP="$QL_HOME/setup.lisp"
if [[ ! -f "$SBCL_SETUP" ]]; then
  echo "[ERROR] Quicklisp setup not found: $SBCL_SETUP" >&2
  exit 1
fi

sbcl --noinform --load "$SBCL_SETUP" --eval '(ql:register-local-projects)' --eval '(quit)'

echo "[QL] Done. Use --force to re-extract and reapply patches."
