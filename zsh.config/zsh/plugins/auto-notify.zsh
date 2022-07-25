#!/usr/bin/env zsh

if which notify-send >/dev/null ; then
    zinit light MichaelAquilina/zsh-auto-notify

    AUTO_NOTIFY_IGNORE+=("config")
    AUTO_NOTIFY_IGNORE+=("zsh" "fish")
    AUTO_NOTIFY_IGNORE+=("e" "edit" "vim" "nvim" "vi" "emacs")
fi
