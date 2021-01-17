#!/usr/bin/env sh

# Create some of the directories that I use and expect to exist.
# This script is a no-op if these already exist.

# Create stub directories for home directory
mkdir -p ~/Desktop ~/Downloads ~/Documents ~/Music ~/Pictures ~/Videos

# Create specific directories used by my scripts/configs/bindings
mkdir -p ~/Pictures/screenshots ~/Videos/screenrecordings
