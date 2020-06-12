#!/usr/bin/env sh

if [ "$EUID" -ne 0 ]; then
    echo "Please run as sudo"
    exit 1
fi

mv /{etc/nixos,tmp}/hardware-configuration.nix
rm -rf /etc/nixos
ln -s "$(realpath nixos)" /etc/nixos
cp /{tmp,etc/nixos}/hardware-configuration.nix

echo hello
