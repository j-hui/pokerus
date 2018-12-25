#!/usr/bin/env bash

TAG="[ALIAS]"

profile="${1:-.bashrc}"

if [[ -w "$profile" ]]; then
	echo "~/$profile doesn't exist!"
	echo "$TAG usage: $0 <target>"
	exit -1
fi

echo "$TAG aliasing to using ~/$profile"

src='
if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases # Pokerus
fi
'

if grep -q '~/.bash_aliases' ~/$profile; then
	echo "$TAG error: ~/$profile seems already aliased:"
    echo
    grep -C 3 '~/.bash_aliases' ~/$profile
	exit -1
else
	echo "$src" >> ~/$profile
fi
