#!/usr/bin/env bash

function focused-window-name() {
    xprop -id $(bspc query -N -n) -f WM_CLASS '8s' ' $1' WM_CLASS \
        | cut -f 2 -d " " | tr -d '"'
}

function keypress () {
    xdotool key --delay 3 --clearmodifiers "$@"
}

key="$1"
[[ -z "$key" ]] && exit 1

echo "KEY=$key; WINDOW=$(focused-window-name)"

case "$key" in
x|c|v)
    case "$(focused-window-name)" in
    kitty|Firefox)
        keypress "super+$key" ;;
    *)
        keypress "ctrl+$key" ;;
    esac
    ;;
*)
    exit 2
    ;;
esac
