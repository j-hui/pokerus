#!/usr/bin/env bash

set -e

sudo nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
sudo nix-channel --add https://nixos.org/channels/nixos-20.09 nixos
sudo nix-channel --update
echo "Added/updated nix channels:"
sudo nix-channel --list
