#!/usr/bin/env bash

path_add() {
    if [ -n "$1" ]; then
        case ":$PATH:" in
            *":$1:"*) :;;           # already there
            *) PATH="$1:$PATH";;    # or PATH="$PATH:$new_entry"
        esac
    fi
}

path_add ~/.local/tms
path_add ~/.local/bin

export FZF_DEFAULT_OPTS='--bind=ctrl-k:kill-line'

if which bat &> /dev/null; then
    export FZF_CTRL_T_OPTS="--preview 'bat --style=numbers --color=always {} | head -500'"
    export MANPAGER="sh -c 'col -bx | bat -l man -p --paging always --style=plain'"
else
    export FZF_CTRL_T_OPTS="--preview 'cat {}'"
fi

if which fd &> /dev/null; then
    export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
    export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
    export FZF_ALT_C_COMMAND="$FZF_DEFAULT_COMMAND"
fi

if which nvim &> /dev/null; then
    export EDITOR=nvim
elif which vim &> /dev/null; then
    export EDITOR=vim
    alias vim='vim'
fi

if which direnv &>/dev/null; then
  eval "$(direnv hook bash)"
fi
 
if [ -n "$KITTY_PID" ] ; then
  # Probably running in a native kitty window.
  #
  # Note that KITTY_WINDOW_ID is defined in both the native kitty window
  # and whatever native kitty SSHs into, so checking that does not work.
  alias ssh='kitten ssh'
fi

if [ -n "${GHOSTTY_RESOURCES_DIR}" ]; then
    builtin source "${GHOSTTY_RESOURCES_DIR}/shell-integration/bash/ghostty.bash"
fi

export RIPGREP_CONFIG_PATH=~/.config/ripgrep/ripgreprc

### OS-specific configuration
case "$OSTYPE" in
*linux*)
    alias ls='ls --color=auto'
    alias rm='rm -I'
    alias dmesg='dmesg --color'
    alias pacman='pacman --color=auto'
    function ls-users() { awk -F':' '{ print \$1}' /etc/passwd; }
    function ls-groups() { awk -F':' '{ print \$1}' /etc/group; }
    ;;
*darwin*)
    alias ls='ls -G'
    # if running the GNU implementation of ls, --color=auto is needed instead.
    # leave this to be reset by .bash_local
    alias nproc='sysctl -n hw.ncpu'
    path_add /opt/homebrew/bin # On Apple Silicon
    ;;
esac

stty -ixon # enable XON/XOFF flow control (whatever that means)

### Navigation/FS

if which lsd &> /dev/null; then
    alias ls='lsd'
    alias ll='lsd -la'
    alias la='lsd -a'
    alias l='lsd -F'
else
    alias ll='ls -lagF'
    alias la='ls -A'
    alias l='ls -F'
fi

alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"

alias cd..='cd ..'

if [ -f ~/.ssh/config ]; then
    complete -W "$(grep 'Host ' ~/.ssh/config | sed 's/Host //')" -X '\*' ssh
fi

# Correct spelling errors in arguments supplied to cd
shopt -s cdspell 2> /dev/null

# For macOS
export BASH_SILENCE_DEPRECATION_WARNING=1

# only works on Bash
if [ "${0#*bash}" != "$0" ]; then
    # Colors
    RED="\[\033[40;0;31m\]"
    GREEN="\[\033[40;0;32m\]"
    YELLOW="\[\033[40;0;33m\]"
    BLUE="\[\033[40;0;34m\]"
    MAGENTA="\[\033[40;0;35m\]"
    CYAN="\[\033[40;0;36m\]"
    LIGHTGRAY="\[\033[40;0;37m\]"
    GRAY="\[\033[40;0;90m\]"
    LIGHTRED="\[\033[40;0;91m\]"
    LIGHTGREEN="\[\033[40;0;92m\]"
    LIGHTYELLOW="\[\033[40;0;93m\]"
    LIGHTBLUE="\[\033[40;0;94m\]"
    LIGHTMAGENTA="\[\033[40;0;95m\]"
    LIGHTCYAN="\[\033[40;0;96m\]"
    CLEAR="\[\033[0m\]"

    # Prompt formatting
    BASE="$BLUE[\A] $LIGHTYELLOW\u$BLUE@$LIGHTCYAN\h$BLUE:$LIGHTRED\w$CLEAR"

    prompt-basic() {
        PS1='\u@\h:\w\$ '
        PS2='> '
    }

    prompt-demo() {
        PS1="\\n$GREEN\u@\h$CLEAR\$ "
        PS2='> '
    }

    prompt-fancier() {
        PS1="\n$BASE \$(if [ \$? != 0 ]; then echo '$RED[ERR]'; else echo '$YELLOW'; fi)\n\$ $CLEAR"
        PS2="$BLUE> $CLEAR"
    }

    # Automatically trim long paths in the prompt (requires Bash 4.x)
    PROMPT_DIRTRIM=3
    prompt-demo

else # ["${0#*bash}" != "$0"]
    PS1='$ '
    PS2='> '
fi # ["${0#*bash}" != "$0"]

# Automatically trim long paths in the prompt (requires Bash 4.x)
PROMPT_DIRTRIM=2

# show "[hh:mm] user@host:pwd" in xterm title bar
if [ "$TERM_PROGRAM" = "Apple_Terminal" ]; then
    # for Mac Terminal, omit "User@Users-MacBook-Air"
    # and preserve PROMPT_COMMAND set by /etc/bashrc.
    show_what_in_title_bar='"[$(date +%H:%M)] ${PWD/#$HOME/~}"'
    PROMPT_COMMAND='printf "\033]0;%s\007" '"$show_what_in_title_bar; $PROMPT_COMMAND"
else
    show_what_in_title_bar='"[$(date +%H:%M)] ${USER}@${HOSTNAME%%.*}:${PWD/#$HOME/~}"'
    case ${TERM} in
        xterm*|rxvt*|Eterm|aterm|kterm|gnome*)
            PROMPT_COMMAND='printf "\033]0;%s\007" '$show_what_in_title_bar
            ;;
        screen)
            PROMPT_COMMAND='printf "\033_%s\033\\" '$show_what_in_title_bar
            ;;
    esac
fi
unset show_what_in_title_bar

### Readline

sbind () { bind "$@" 2>/dev/null ; }

# Enable history expansion with space
# E.g. typing !!<space> will replace the !! with your last command
sbind Space:magic-space

# Perform file completion in a case insensitive fashion
sbind "set completion-ignore-case on"

# Treat hyphens and underscores as equivalent
sbind "set completion-map-case on"

# Display matches for ambiguous patterns at first tab press
sbind "set show-all-if-ambiguous on"

### History

# Append to the history file, don't overwrite it
shopt -s histappend

# Save multi-line commands as one command
shopt -s cmdhist

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

# Huge history
HISTSIZE=500000
HISTFILESIZE=100000

# Avoid duplicate entries
HISTCONTROL="erasedups:ignoreboth"

# Don't record some commands
export HISTIGNORE="&:[ ]*:exit:ls:ll:bg:fg:history:clear"

# Use standard ISO 8601 timestamp
# %F equivalent to %Y-%m-%d
# %T equivalent to %H:%M:%S (24-hours format)
HISTTIMEFORMAT='%F %T '

# Enable incremental history search with up/down arrows (also Readline goodness)
# Learn more about this here: http://codeinthehole.com/writing/the-most-important-command-line-tip-incremental-history-searching-with-inputrc/
sbind '"\e[A": history-search-backward'
sbind '"\e[B": history-search-forward'
sbind '"\e[C": forward-char'
sbind '"\e[D": backward-char'

# Record each line as it gets issued
PROMPT_COMMAND='history -a'

unset sbind

if [ -f ~/.bash_local ]; then
     source ~/.bash_local
fi

# vim:fileencoding=utf-8:ft=sh
