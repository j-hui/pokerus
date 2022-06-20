#!/usr/bin/env zsh

edit () {
  "$EDITOR" "$@"
}

alias e='edit'
alias nv='nvim'

if (( $+commands[exa] )); then
  exa_params=('--git' '--icons' '--classify' '--group-directories-first' '--time-style=long-iso' '--group' '--color-scale')

  alias ls='exa ${exa_params}'
  alias l='exa --git-ignore ${exa_params}'
  alias ll='exa --all --header --long ${exa_params}'
  alias llm='exa --all --header --long --sort=modified ${exa_params}'
  alias la='exa -lbhHigUmuSa'
  alias lx='exa -lbhHigUmuSa@'
  alias lt='exa --tree'
  alias tree='exa --tree'
else
  if [[ "$(uname)" != Darwin ]]; then
    alias ls='ls --color=auto'
  fi
fi

if [[ "$(uname)" == Darwin ]]; then
  alias o='open'
else
  if (( $+commands[mimeo] )); then
    alias o=mimeo
  else
    alias o=xdg-open
  fi
fi

# Taken from https://github.com/momo-lab/zsh-replace-multiple-dots/blob/master/replace-multiple-dots.plugin.zsh
# function replace_multiple_dots() {
#   local dots=$LBUFFER[-3,-1]
#   if [[ $dots =~ "^[ //\"']?\.\.$" ]]; then
#     LBUFFER=$LBUFFER[1,-3]'../.'
#   fi
#   zle self-insert
# }
#
# zle -N replace_multiple_dots
# bindkey "." replace_multiple_dots

alias .=pwd
for i in .. ... .... ..... ; do
    eval "alias $i='cd $i'"
done
