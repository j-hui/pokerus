#!/usr/bin/env bash

set -e

if [ -e /etc/nixos/pokerus ]; then

    echo "/etc/nixos/pokerus already exists:"
    ls -la /etc/nixos/pokerus
    exit 1
fi

sudo ln -s ~/.config/nixos /etc/nixos/pokerus

echo "Successfully linked pokerus:"
ls -la /etc/nixos/pokerus

if [ -e /persist/etc/nixos/ ]; then

    sudo ln -s ~/.config/nixos /persist/etc/nixos/pokerus
    echo "Also linked to /persist/etc/nixos, and updated configs"

    ls -la /persist/etc/nixos/pokerus
else
    echo "No /persist/etc/nixos, did nothing to that."
fi

