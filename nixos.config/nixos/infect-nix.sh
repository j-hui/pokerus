#!/usr/bin/env sh

if [ -e /etc/nixos/pokerus ]; then

    echo "/etc/nixos/pokerus already exists:"
    ls -la /etc/nixos/pokerus
    exit 1
fi

sudo ln -s ~/.config/nixos /etc/nixos/pokerus

echo "Successfully linked pokerus:"
ls -la /etc/nixos/pokerus
