#!/usr/bin/env zsh

edit () {
  "$EDITOR" "$@"
}

alias e='edit'
alias nv='nvim'

if (( $+commands[lsd] )); then
  alias ls='lsd'
  alias ll='lsd -la'
  alias la='lsd -a'
  alias l='lsd -F'
  alias tree='lsd --tree'
else
  if [[ "$(uname)" != Darwin ]]; then
    alias ls='ls --color=auto'
  fi
  alias ll='ls -lagF'
  alias la='ls -A'
  alias l='ls -F'
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
