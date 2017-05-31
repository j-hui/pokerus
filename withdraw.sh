#!/usr/bin/env bash

if [[ "$#" -lt 1 ]]; then
	echo "usage: $0 [<boxes> ...]"
	exit -1
fi

for dir in "$@"; do
	stow "$dir" --target="$HOME" --ignore='\.DS_Store'
done
