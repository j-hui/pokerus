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
