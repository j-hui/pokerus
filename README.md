# Pokerus

Maintaining my dot files across macOS and Linux.
This `README.md` contains some handy notes for setting things up.

## Basic setup

First, install these core dependencies:

    sudo git bash

Then, retrieve this repo:

    git clone git@github.com:j-hui/pokerus.git ~/pokerus && cd ~/pokerus

Optionally, disinfect your system (warning: not well-tested):

    ./disinfect.sh

Install Bash hook, and setup Git and Bash:

    ./alias.sh && ./infect.sh bash git

### Common setups

Full terminal rig:

    ./infect.sh vim nvim.config tmux ranger.config ghc

Darwin/Aqua rig:

    ./infect.sh yabai.config skhd.config qutebrowser

Linux/X11 rig:

    ./infect.sh bspwm.config sxhkd.config # TODO: <bar> <laucher> <gtk> <qutebrowser>

Doom Emacs rig:

    ./infect.sh doom-emacs
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d && \
        ~/.emacs.d/bin/doom install

If `install` fails, running it a second term fixes it for some reason..

## Additional setup

### Git user

    git config --global user.email
    git config --global user.name

### Password-less `sudo`

Run `sudo visudo`, and stick this at the bottom:

    <user> ALL=(ALL) NOPASSWD:ALL

_Not recommended for machines where security is a priority._

## System-specific setup

- [Darwin](darwin-setup.md)
- [Debian](debian-setup.md)
- [Linux Kernel Development](linux-dev.md)
- [Key Bindings Overview](keybinds.md)
