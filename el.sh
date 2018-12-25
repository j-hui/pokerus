
#!/usr/bin/env bash

TAG="[EMACS_INIT]"

profile="${1:-.emacs}"

if [[ -w "$profile" ]]; then
	echo "~/$profile doesn't exist!"
	echo "$TAG usage: $0 <target>"
	exit -1
fi

echo "$TAG aliasing to using ~/$profile"

src='(load "~/.pokerus.el")'
sexpr="
(package-refresh-contents)
(package-install 'paradox)
"

if grep -q '~/.pokerus.el' ~/$profile; then
	echo "$TAG error: ~/$profile seems already initialized:"
    echo
    grep -C 3 '~/.pokerus.el' ~/$profile
	exit -1
else
	echo "$src" >> ~/$profile
    emacs --eval="$sexpr"
fi
