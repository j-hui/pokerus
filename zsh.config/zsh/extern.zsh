#!/usr/bin/env zsh

if [[ -z $EDITOR ]]; then
  if (( $+commands[nvim] )) ; then
    export EDITOR=nvim
  else
    export EDITOR=vim
  fi
fi

if (( $+commands[bat] )); then
  export MANPAGER="sh -c 'col -bx | bat -l man -p --paging always --style=plain'"
fi

export FZF_DEFAULT_OPTS='--bind=ctrl-k:kill-line,alt-a:select-all,alt-e:deselect-all,ctrl-space:toggle --marker=*'

if (( $+commands[bat] )); then
  export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {} | head -500'"
else
  export FZF_CTRL_T_OPTS="--preview 'cat {}'"
fi

if (( $+commands[fd] )); then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
fi

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/ripgreprc

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
    alias open=mimeo
  else
    alias o=xdg-open
    alias open=xdg-open
  fi
fi

if (( $+commands[direnv] )) ; then
  # This probably needs to happen after PS1 is set
  eval "$(direnv hook zsh)"
fi

[ -f ~/.local/share/git-subrepo/.rc ] && source ~/.local/share/git-subrepo/.rc
