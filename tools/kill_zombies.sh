#!/bin/bash

echo "🧟 Killing zombie processes..."

pkill -9 -u $USER -f "sbcl" 2>/dev/null || true
pkill -9 -u $USER -f "guardian" 2>/dev/null || true
pkill -9 -u $USER -f "data_keeper.py" 2>/dev/null || true
pkill -9 -u $USER -f "notifier.py" 2>/dev/null || true
pkill -9 -u $USER -f "evolution_daemon.py" 2>/dev/null || true
pkill -9 -u $USER -f "strategy_hunter.py" 2>/dev/null || true

if command -v systemctl >/dev/null 2>&1; then
  if [ "${EUID:-$(id -u)}" -eq 0 ]; then
    systemctl reset-failed || true
  elif command -v sudo >/dev/null 2>&1 && sudo -n true >/dev/null 2>&1; then
    sudo -n systemctl reset-failed || true
  fi
fi

echo "💀 Zombies neutralized."
