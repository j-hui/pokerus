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
path_add ~/.cargo/bin
path_add ~/go/bin
path_add ~/.local/bin
path_add ~/.cabal/bin
path_add ~/.ghcup/bin
path_add ~/.yarn/bin
path_add ~/.config/yarn/global/node_modules/.bin
path_add /snap/bin
path_add ~/.local/go/bin
path_add ~/.platformio/penv/bin
