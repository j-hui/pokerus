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

# Taken from https://github.com/momo-lab/zsh-replace-multiple-dots/blob/master/replace-multiple-dots.plugin.zsh
function replace_multiple_dots() {
  local dots=$LBUFFER[-3,-1]
  if [[ $dots =~ "^[ //\"']?\.\.$" ]]; then
    LBUFFER=$LBUFFER[1,-3]'../.'
  fi
  zle self-insert
}

zle -N replace_multiple_dots
bindkey "." replace_multiple_dots
