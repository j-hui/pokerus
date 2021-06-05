#!/usr/bin/env bash

# Prepares the system to be made stateless according to the darling

set -e

# Pretty-print progress
prog () {
    echo "[ POKERUS // NIX ]" "$@"
}

cp-rm-link () {
    src="$1"
    dst="$2"
    if ! [ -e "$src" ]; then
        echo "Source $src does not exist."
        return 1
    fi

    if [ -e "$dst" ]; then
        echo "Something already exists at destination $dst."
        return 1
    fi

    if [ -d "$src" ]; then
        cp -r "$src" "$dst"
        rm -rf "$src"
        ln -s "$dst" "$src"
    else
        cp "$src" "$dst"
        rm -f "$src"
        ln -s "$dst" "$src"
    fi
}

prog "Basic sanity checks for settings..." # {{{
if [ $(id -u) -ne 0 ]; then
    prog "Please re-run as root."
    exit 1
fi

if ! [ -d /home/j-hui/persist/ ]; then
    prog "/home/j-hui/persist/ does not appear to exist, or isn't a directory."
    exit 1
fi

prog "LGTM."
sleep 1 # }}}

echo "WARNING: Untested script."
echo "Note to self: review this script before running, delete these lines to"
echo
echo "run the script, and commit the change if successful."
echo "Consider backing up files before doing so."
exit 1

mkdir -p /home/j-hui/persist/etc/nixos
cp-rm-link {,/home/j-hui/persist}/etc/NIXOS
cp-rm-link {,/home/j-hui/persist}/etc/nixos/configuration.nix
cp-rm-link {,/home/j-hui/persist}/etc/nixos/hardware-configuration.nix

echo "WARNING: Untested, and unclear if NetworkManger-related files will be"
echo "present on machines that don't use Wifi."
echo
echo "Note to self: review this script before running, delete these lines to"
echo "run the script, and commit the change if successful."
echo "Consider backing up files before doing so."
exit 1

mkdir -p /home/j-hui/persist/etc/NetworkManager
cp-rm-link {,/home/j-hui/persist}/etc/NetworkManager/system-connections

mkdir -p /home/j-hui/persist/var/lib/NetworkManager
cp-rm-link {,/home/j-hui/persist}/var/lib/NetworkManager/secret_key
cp-rm-link {,/home/j-hui/persist}/var/lib/NetworkManager/seen-bssids
cp-rm-link {,/home/j-hui/persist}/var/lib/NetworkManager/timestamps

# vim: set ts=4 sw=0 tw=80 et foldmethod=marker:
