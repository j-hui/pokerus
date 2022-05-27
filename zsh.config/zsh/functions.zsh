#!/usr/bin/env zsh

path_add () {
    if [ -n "$1" ]; then
        case ":$PATH:" in
            *":$1:"*) :;;           # already there
            *) PATH="$1:$PATH";;    # or PATH="$PATH:$new_entry"
        esac
    fi
}

path_add ~/.local/tms
