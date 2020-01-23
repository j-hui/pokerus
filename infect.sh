#!/usr/bin/env bash

TAG="[POKERUS]"

if [[ "$#" -lt 1 ]]; then
	echo "usage: $0 [<boxes> ...]"
	exit -1
fi

for dir in "$@"; do
	if [[ ! -d "$dir" ]]; then
		echo "$TAG $dir is not a directory"
	elif [[ "${dir##*.}" == "immune" || "${dir##*.}" == "immune/" ]]; then
		echo "$TAG ignoring $dir"
	elif [[ "${dir##*.}" == "config" || "${dir##*.}" == "config/" ]]; then
		if stow -vv "$dir" --target="$HOME/.config" --ignore='\.DS_Store'; then
			echo "$TAG infected $HOME/.config with $dir"
		else
			echo "$TAG encountered error infecting $dir"
		fi
	else
		if stow -vv "$dir" --target="$HOME" --ignore='\.DS_Store'; then
			echo "$TAG infected $HOME with $dir"
		else
			echo "$TAG encountered error infecting $dir"
		fi
	fi
done
