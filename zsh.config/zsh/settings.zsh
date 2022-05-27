#!/usr/bin/env zsh

setopt BAD_PATTERN

unsetopt BEEP
unsetopt LIST_BEEP

SHELL=zsh

# From https://jdhao.github.io/2021/03/24/zsh_history_setup/
export HISTFILE=~/.histfile
export HISTSIZE=1000000   # the number of items for the internal history list
export SAVEHIST=1000000   # maximum number of items for the history file

# The meaning of these options can be found in man page of `zshoptions`.
setopt HIST_IGNORE_ALL_DUPS  # do not put duplicated command into history list
setopt HIST_SAVE_NO_DUPS  # do not save duplicated command
setopt HIST_REDUCE_BLANKS  # remove unnecessary blanks
setopt INC_APPEND_HISTORY_TIME  # append command to history file immediately after execution
setopt EXTENDED_HISTORY  # record command start time

alias history="fc -l 1"
