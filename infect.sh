#!/usr/bin/env bash

TAG="[POKERUS]"

if [[ "$#" -lt 1 ]]; then
	echo "usage: $0 [<boxes> ...]"
	exit -1
fi

for dir in "$@"; do
	if [[ -d "$dir" ]]; then
		stow "$dir" --target="$HOME" --ignore='\.DS_Store'
		echo "$TAG infected $HOME with $dir"
	else
		echo "$TAG $dir is not a directory"
	fi
done
