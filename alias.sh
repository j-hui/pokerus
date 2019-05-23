#!/usr/bin/env bash

TAG="[ALIAS]"

profile="${1:-.bashrc}"

if [[ -w "$profile" ]]; then
	echo "~/$profile doesn't exist!"
	echo "$TAG usage: $0 <target>"
	exit -1
fi

echo "$TAG aliasing to using ~/$profile"

append() {
    local dst=$1
    local key=$2
    local src=$3
    
    if grep -q "$key" "$dst"; then
        echo "$TAG error: $dst seems already aliased:"
        echo
        grep -C 3 "$key" "$dst"
        return 1
    else
        echo "$src" >> "$dst"
        echo "$TAG: $dst aliased"
    fi
}

append ~/$profile '~/.bash_aliases' '
if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases # Pokerus
fi
'

append ~/.gitconfig '~/.pokerus.git' '
[include]
    path = ~/.gitpokerus
'
