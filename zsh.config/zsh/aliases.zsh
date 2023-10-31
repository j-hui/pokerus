#!/usr/bin/env zsh

edit () {
  "$EDITOR" "$@"
}

alias e='edit'
alias nv='nvim'

if (( $+commands[lsd] )); then
  lsd_params=('--git' '--icons' '--classify' '--group-directories-first' '--time-style=long-iso' '--group' '--color-scale')

  alias ls='lsd ${lsd_params}'
  alias l='lsd --git-ignore ${lsd_params}'
  alias ll='lsd --all --header --long ${lsd_params}'
  alias llm='lsd --all --header --long --sort=modified ${lsd_params}'
  alias la='lsd -lbhHigUmuSa'
  alias lx='lsd -lbhHigUmuSa@'
  alias lt='lsd --tree'
  alias tree='lsd --tree'
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
