#!/usr/bin/env zsh

# Emacs-style keybinds
bindkey -e

# Delete until /, like bash
# See https://zsh.sourceforge.io/Doc/Release/User-Contributions.html#Widgets
autoload -U select-word-style
select-word-style bash

# ctrl-{left,right,up,down}
bindkey "^[[1;5C" forward-word
bindkey "^[[1;5D" backward-word
bindkey "^[[1;5A" beginning-of-line
bindkey "^[[1;5B" end-of-line

# alt-{left,right,up,down}
bindkey '\e\e[C' forward-word
bindkey '\e\e[D' backward-word 
bindkey '\e\e[C' beginning-of-line
bindkey '\e\e[D' end-of-line

# From: https://stackoverflow.com/a/4766798
# This was written entirely by Mikael Magnusson (Mikachu)
# Basically type '...' to get '../..' with successive .'s adding /..
function rationalise-dot {
    local MATCH # keep the regex match from leaking to the environment
    if [[ $LBUFFER =~ '(^|/| |      |'$'\n''|\||;|&)\.\.$' ]]; then
      LBUFFER+=/
      zle self-insert
      zle self-insert
    else
      zle self-insert
    fi
}
zle -N rationalise-dot
bindkey . rationalise-dot
# without this, typing a . aborts incremental history search
bindkey -M isearch . self-insert

