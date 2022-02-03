#!/usr/bin/env bash

pkill polybar || true
while pgrep polybar >/dev/null; do sleep 1; done

for m in $(polybar --list-monitors | cut -d":" -f1); do
    MONITOR=$m polybar --reload top &
    echo "Launched polybar top for $m ($!)"
    MONITOR=$m polybar --reload bottom &
    echo "Launched polybar bottom for $m ($!)"
done
