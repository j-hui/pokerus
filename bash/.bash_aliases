export EDITOR=vim

alias sudo='sudo '
alias json="python -m json.tool"

errecho() {
	echo "Error: $@" >&2
}

case "$OSTYPE" in
*linux*)
	alias dmesg='dmesg --color'
	alias pacman='pacman --color=auto'
	alias ls='ls --color=auto'
	;;
*darwin*)
	alias ls='ls -G'
	;;
esac

which say &> /dev/null && alias talk='cat - | while read cat; do say $cat; done'

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

alias ll='ls -lagF'
alias la='ls -A'
alias l='ls -CF'

alias cd..='cd ..'
alias ..='cd ..'
alias ...='cd ../../'

alias usage='du -h -d1'

# grep color
alias grep="grep --color=auto"
alias egrep="egrep --color=auto"
alias fgrep="fgrep --color=auto"
alias bell="echo -e '\a'"

function backup() {
        mv $1{,.backup}
}

function restore() {
        mv ${1%*.backup}{.backup,}
}


[ -z "$PS1" ] && return
# color prompt
GREEN="\[\033[40;0;32m\]"
RED="\[\033[40;0;31m\]"
YELLOW="\[\033[40;0;33m\]"
CYAN="\[\033[40;1;36m\]"
BLUE="\[\033[40;0;34m\]"
GRAY="\[\033[40;0;37m\]"
CLEAR="\[\033[0m\]"
BASE="$GRAY[$RED\u$YELLOW@$BLUE\h:$YELLOW\W$GRAY]"
NERR="\[\033[40;0;33m\]8==D\[\033[40;0;36m\]~"
ERR="\[\033[40;0;31m\]D==\[\033[40;0;36m\]8\[\033[40;0;31m\]<"

SFW_NERR='\[\033[40;0;33m\]$'
SFW_ERR='\[\033[40;0;31m\]D:'

BR='############################'

export PS1="$BASE \$(if [[ \$? != 0 ]]; then echo '$ERR'; else echo '$NERR'; fi) $CLEAR"
export PS2="\[\033[40;0;34m\]8==D \[\033[0m\]"

basic() {
	PS1='\[\e]0;\u@\h: \w\a\]${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
	PS2='> '
}
sfw() {
    PS1="$BASE \$(if [[ \$? != 0 ]]; then echo '$SFW_ERR'; else echo '$SFW_NERR'; fi) $CLEAR"
    PS2="\[\033[40;0;34m\]8==D \[\033[0m\]"
}

nsfw() {
    PS1="$BASE \$(if [[ \$? != 0 ]]; then echo '$ERR'; else echo '$NERR'; fi) $CLEAR"
    PS2="\[\033[40;0;34m\]8==D \[\033[0m\]"
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

modbash() {
	if [ $# -eq 1 ]; then 
		if [ "$1" == a ]; then $EDITOR ~/.bash_aliases
		elif [ "$1" == l ]; then $EDITOR ~/.bash_local
		else $EDITOR ~/.bash_$1
		fi
	elif [ $# -gt 1 ]; then errecho "Too many arguments: $@"; return 1
	elif [ -w ~/.bash_local ]; then $EDITOR ~/.bash_local
	elif [ -w ~/.bash_aliases ]; then $EDITOR ~/.bash_aliases
        elif [ -w ~/.bashrc ]; then $EDITOR ~/.bashrc
        elif [ -w ~/.bash_profile ]; then $EDITOR ~/.bash_profile
	else errecho 'No ~/.bashrc_aliases ~/.bashrc, or ~/.bash_profile found.'; return 1
	fi	
}

srcbash() {
	if [ -e ~/.bashrc ]; then source ~/.bashrc
	elif [ -e ~/.bash_profile ]; then source ~/.bash_profile
	else errecho 'No ~/.bashrc or ~/.bash_profile found.'; return 1
	fi
}

ssh-conf() {
	if [ -w ~/.ssh/config ]; then $EDITOR ~/.ssh/config
	else errecho 'No ~/.ssh/config found.'; return 1
	fi
}

ssh-export() {
	[ $# -lt 1 ] && errecho "Usage: $0 []" && return 1
	for host in "$@"; do
		ssh ${host} > /dev/null <<-EOF
			set -e 
			[ -d ~/.ssh ] || mkdir ~/.ssh
			echo "# $(hostname)" >> ~/.ssh/authorized_keys
			echo "$(cat ~/.ssh/id_rsa.pub)" >> ~/.ssh/authorized_keys
EOF
	done
}

ssh-bootstrap() {
	[ $# -lt 1 ] && errecho "Usage: $0 []" && return 1
	for host in "$@"; do
		ssh ${host} > /dev/null <<-EOF
			set -e
			ssh -o StrictHostKeyChecking=no git@github.com
			(cd ~ && git clone git@github.com:j-hui/dot-files.git .__tmp && mv .__tmp/.git . && rm -rf .__tmp && git reset --hard)
			./.init-dot.sh
			./.init-git-config.sh
EOF
	done
}

alias :w="echo You\'re not in vim, doofus."
alias mrproper="rm -rvf .*.swp"
alias f="find . -name"

rgrep() {
	grep -r "$@" .
}

if [ -f ~/.ssh/config ]; then
	complete -W "$(grep 'Host ' ~/.ssh/config | sed 's/Host //')" -X '\*' ssh
fi

if [ -f ~/.bash_local ]; then
    . ~/.bash_local
fi
