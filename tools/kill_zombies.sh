#!/bin/bash

echo "🧟 Killing zombie processes..."

pkill -9 -u $USER -f "sbcl" 2>/dev/null || true
pkill -9 -u $USER -f "guardian" 2>/dev/null || true
pkill -9 -u $USER -f "data_keeper.py" 2>/dev/null || true
pkill -9 -u $USER -f "notifier.py" 2>/dev/null || true
pkill -9 -u $USER -f "evolution_daemon.py" 2>/dev/null || true
pkill -9 -u $USER -f "strategy_hunter.py" 2>/dev/null || true

systemctl --user reset-failed

echo "💀 Zombies neutralized."
