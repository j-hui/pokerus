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
