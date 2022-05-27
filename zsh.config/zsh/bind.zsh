#!/usr/bin/env zsh

bindkey -e

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
