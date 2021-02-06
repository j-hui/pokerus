#!/usr/bin/env sh
# Set up my home directory, prior to infecting.
# This script is idempotent; running it additional times should be ok.
set -e

echo "Info: Making sure home directories exist."
# Create stub directories for home directory
mkdir -p -v ~/Desktop ~/Downloads ~/Documents ~/Music ~/Pictures ~/Videos
# Create specific directories used by my scripts/configs/bindings
mkdir -p -v ~/Pictures/screenshots ~/Videos/screenrecordings
echo

# grep for any indication (key) that we've already appended src to dst
append () {
    local dst=$1
    local key=$2
    local src=$3

    echo "Info: appending $key... >> $dst"
    if grep -q "$key" "$dst"; then
        echo "Warning: $dst already seems aliased:"
        echo
        grep -C 3 "$key" "$dst"
    else
        echo "$src" >> "$dst"
        echo "Info: appended $key... >> $dst."
    fi
    echo
}


append ~/.bashrc '~/.bash_aliases' '
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases # Pokerus
fi
'

append ~/.gitconfig '~/.gitpokerus' '
[include]
    path = ~/.gitpokerus
'

echo "Info: sourcing .bash_aliases from ~/.bashrc and ~/.bash_profile"
if test -e ~/.bash_profile; then
    if test $(readlink ~/.bash_profile) = '.bashrc'; then
        echo "Warning: already linked ~/.bash_profile -> .bashrc"
    else
        echo "Warning: ~/.bash_profile already exists, with the following content: "
        head -n 30 ~/.bash_profile
        echo "(capped at 30 lines)"
        echo
        echo "Attempting to remove it."
        rm -i ~/.bash_profile
        if test -e ~/.bash_profile; then
            echo "Warning: ~/.bash_profile still exists. Not linking ~/.bash_profile -> .bashrc."
        else
            ln -s .bashrc ~/.bash_profile
            echo "Info: Linked ~/.bash_profile -> .bashrc."
        fi
    fi
else
    ln -s .bashrc ~/.bash_profile
    echo "Info: Linked ~/.bash_profile -> .bashrc."
fi

echo "Home directory is all set and ready for infection."
