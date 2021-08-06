
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

install_paradox() {
    emacs --eval="(package-refresh-contents)"
    emacs --eval="(package-install 'paradox)"
}

if grep -q '~/.pokerus.el' ~/$profile; then
	echo "$TAG error: ~/$profile seems already initialized:"
    echo
    grep -C 3 '~/.pokerus.el' ~/$profile
    echo "$TAG attempting to install paradox any way"
    install_paradox
	exit -1
else
	echo "$src" >> ~/$profile
    emacs --eval="$sexpr"
    install_paradox
fi
