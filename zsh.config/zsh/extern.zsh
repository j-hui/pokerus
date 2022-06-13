#!/usr/bin/env zsh

# [ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export FZF_DEFAULT_OPTS='--bind=ctrl-k:kill-line,alt-a:select-all,alt-e:deselect-all,ctrl-space:toggle --marker=*'

if (( $+commands[bat] )); then
  export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {} | head -500'"
  export MANPAGER="sh -c 'col -bx | bat -l man -p --paging always'"
  # NOTE: MANPAGER will be later overridden if nvim is installed
else
  export FZF_CTRL_T_OPTS="--preview 'cat {}'"
fi

if [[ -z $EDITOR ]]; then
  if (( $+commands[nvim] )) ; then
    export EDITOR=nvim
  else
    export EDITOR=vim
  fi
fi

if (( $+commands[nvim] )) ; then
  export MANPAGER='nvim +Man!'
  export MANWIDTH=999
fi

export ZK_NOTEBOOK_DIR=~/canalave
