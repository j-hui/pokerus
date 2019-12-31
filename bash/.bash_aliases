errecho() {
    echo "Pokerus error: $@" >&2
}

PATH_ADD() {
    if [[ "$1" ]]; then
        case ":$PATH:" in
            *":$1:"*) :;;           # already there
            *) PATH="$1:$PATH";;    # or PATH="$PATH:$new_entry"
        esac
    fi
}

### Vim support
export EDITOR=vim
mkdir -p ~/.tmp/backup ~/.tmp/swp ~/.tmp/undo

### OS-specific configuration
case "$OSTYPE" in
*linux*)
    alias dmesg='dmesg --color'
    alias pacman='pacman --color=auto'
    # alias ls='ls --color=auto'
    ;;
*darwin*)
    # alias ls='ls -G'
    alias nproc='sysctl -n hw.ncpu'
    ;;
esac

stty -ixon # enable XON/XOFF flow control (whatever that means)

### Handy aliases

## Navigation/FS
alias ls='ls --color=auto'
alias ll='ls -lagF'
alias la='ls -A'
alias l='ls -CF'

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'

cs() {
    cd "$@" && ls
}

alias sl='echo "
                 _-====-__-======-__-========-_____-============-__
               _(                                                 _)
            OO(           _/_ _  _  _/_   _/_ _  _  _/_           )_
           0  (_          (__(_)(_) (__   (__(_)(_) (__            _)
         o0     (_                                                _)
        o         -=-___-===-_____-========-___________-===-dwb-=-
      .o                                _________
     . ______          ______________  |         |      _____
   _()_||__|| ________ |            |  |_________|   __||___||__
  (BNSF 1995| |      | |            | __Y______00_| |_         _|
 /-OO----OO^^=^OO--OO^=^OO--------OO^=^OO-------OO^=^OO-------OO^=P
#####################################################################"'

## Searching
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
rgrep() {
    grep -r "$@" .
}
alias f="find . -name"

## Utilities
alias usage='du -h -d1'
alias mrproper="rm -rvf .*.swp *~"
alias dfob="perl -pi -e 's/[^[:ascii:]]//g'"
alias json="python -m json.tool"

## Misc
alias sudo='sudo ' # helps with scripting?
alias rm='rm -I'
alias stonks='stack'
alias :w="echo You\'re not in vim, dingus."

### Pokeconfig aliases
modbash() {
    local rc_file
    if [ $# -eq 1 ]; then
        if [ "$1" == a ];           then rc_file=~/.bash_aliases
        elif [ "$1" == l ];         then rc_file=~/.bash_local
        else rc_file=~/.bash_$1
        fi
    elif [ $# -gt 1 ]; then errecho "Too many arguments: $@"; return 1
    elif [ -w ~/.bash_local ];      then rc_file=~/.bash_local
    elif [ -w ~/.bash_aliases ];    then rc_file=~/.bash_aliases
    elif [ -w ~/.bashrc ];          then rc_file=~/.bashrc
    elif [ -w ~/.bash_profile ];    then rc_file=~/.bash_profile
    else errecho 'No ~/.bashrc{_aliases,rc,_profile} found.'; return 1
    fi
    $EDITOR "${EDIT_BASH[@]}" $rc_file
}

srcbash() {
    if [ -e ~/.bashrc ]; then source ~/.bashrc
    elif [ -e ~/.bash_profile ]; then source ~/.bash_profile
    else errecho 'No ~/.bashrc or ~/.bash_profile found.'; return 1
    fi
}

pokepull() {(
    cd ~/pokerus && git pull
)}

ssh-conf() {
    if [ -w ~/.ssh/config ]; then
        $EDITOR ~/.ssh/config
    else
        errecho "No ~/.ssh/config found."
        errecho "Trying to touch ~/.ssh/config, try running $0 again."
        touch ~/.ssh/config
        return 1
    fi
}

if [ -f ~/.ssh/config ]; then
    complete -W "$(grep 'Host ' ~/.ssh/config | sed 's/Host //')" -X '\*' ssh
fi

### Prompt/display configuration
# [ -z "$PS1" ] && return

# Colors
GREEN="\[\033[40;0;32m\]"
RED="\[\033[40;0;31m\]"
YELLOW="\[\033[40;0;33m\]"
CYAN="\[\033[40;1;36m\]"
BLUE="\[\033[40;0;34m\]"
GRAY="\[\033[40;0;37m\]"
CLEAR="\[\033[0m\]"

# Prompt formatting
BASE="$GRAY[$RED\u$YELLOW@$BLUE\h:$YELLOW\W$GRAY]"
NERR="\[\033[40;0;33m\]8==D\[\033[40;0;36m\]~"
ERR="\[\033[40;0;31m\]D==\[\033[40;0;36m\]8\[\033[40;0;31m\]<"
SFW_NERR='\[\033[40;0;33m\]$'
SFW_ERR='\[\033[40;0;31m\]D:'
BR='############################'

basic() {
    export PS1='\u@\h:\w\$ '
    export PS2='> '
}
sfw() {
    export PS1="$BASE \$(if [[ \$? != 0 ]]; then echo '$SFW_ERR'; else echo '$SFW_NERR'; fi) $CLEAR"
    export PS2="\[\033[40;0;34m\]> \[\033[0m\]"
}
nsfw() {
    export PS1="$BASE \$(if [[ \$? != 0 ]]; then echo '$ERR'; else echo '$NERR'; fi) $CLEAR"
    export PS2="\[\033[40;0;34m\]8==D \[\033[0m\]"
}

# colored man pages
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;31m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
            man "$@"
}

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

sfw

### Jump to ~/.bash_local
if [ -f ~/.bash_local ]; then
    . ~/.bash_local
fi
