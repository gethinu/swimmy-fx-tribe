#!/usr/bin/env bash
# ============================================================================
# (A) daemon-isolation verification — Ubuntu-22.04 userspace bootstrap
# ----------------------------------------------------------------------------
# Stands up the FULL swimmy Common Lisp system in a NON-live WSL distro with
# NO sudo: userspace SBCL + vendored libzmq (+ system libsqlite3) so
# `asdf:load-system :swimmy` loads, then runs daemon_verify.lisp against a
# COPY of swimmy.db. No live orders, no guardian daemon, live DB untouched.
#
# Prereq (Windows side, before this):
#   - copy data/memory/swimmy.db -> C:\tmp\swimmy_daemon_verify.db  (remove any
#     stale -wal/-shm sidecars first, else sqlite reports "malformed").
# Run (from an Ubuntu-22.04 shell that mounts C: at /mnt/c):
#   bash wsl_env_setup.sh          # one-time: fetch+install SBCL/libzmq+quicklisp
#   source ~/swenv.sh && cd /mnt/c/Repos/swimmy-fx-tribe \
#     && sbcl --dynamic-space-size 5120 --non-interactive --load ~/run_harness.lisp
# ============================================================================
set -eu
ROOT=~/local/root
mkdir -p ~/dl "$ROOT"
cd ~/dl

echo "== fetch debs (no sudo; apt sandbox override) =="
for p in sbcl sbcl-source libzmq5 libzmq3-dev libpgm-5.3-0 libnorm1; do
  apt-get download -o APT::Sandbox::User=root "$p" 2>&1 | tail -1
done
for d in *.deb; do dpkg-deb -x "$d" "$ROOT"; done

LIBDIR="$ROOT/usr/lib/x86_64-linux-gnu"
INC="$ROOT/usr/include"
[ -f "$LIBDIR/libzmq.so.5" ] && [ ! -e "$LIBDIR/libzmq.so" ] && ln -sf libzmq.so.5 "$LIBDIR/libzmq.so"

cat > ~/swenv.sh <<EOF
export SBCL_HOME="$ROOT/usr/lib/sbcl"
export PATH="$ROOT/usr/bin:\$PATH"
export LD_LIBRARY_PATH="$LIBDIR:\${LD_LIBRARY_PATH:-}"
export C_INCLUDE_PATH="$INC:\${C_INCLUDE_PATH:-}"    # cffi-grovel needs zmq.h
export CPATH="$INC:\${CPATH:-}"
export LIBRARY_PATH="$LIBDIR:\${LIBRARY_PATH:-}"
EOF
source ~/swenv.sh
echo "== SBCL: $(sbcl --version) =="

echo "== quicklisp bootstrap =="
if [ ! -f ~/quicklisp/setup.lisp ]; then
  curl -sSLo ~/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
  sbcl --non-interactive --load ~/quicklisp.lisp --eval '(quicklisp-quickstart:install)' --eval '(sb-ext:quit)'
fi
echo "== ready: quickload \"swimmy\" pulls pzmq(->libzmq)/sqlite(->system)/jsown/... =="
# NOTE: guardian's own Rust rebuild in this distro is blocked (zmq-sys builds
# libzmq from source -> needs g++/pkg-config which need sudo); the harness uses
# the prebuilt Windows target/release/primitive_scan.exe (guardian's engine) via
# WSL interop instead. See tribe_2a_daemon_isolation_20260718.md §3(4).
