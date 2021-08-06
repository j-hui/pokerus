# Pokerus

Maintaining my dot files across macOS and Linux.
This `README.md` contains some handy notes for setting things up.

## Basic setup

First, install these core dependencies:

    sudo git bash

Then, retrieve this repo:

    git clone git@github.com:j-hui/pokerus.git ~/pokerus && cd ~/pokerus

Set up the git aliases:

    git config --local include.path ../.gitconfig

Optionally, disinfect your system (warning: not well-tested):

    git disinfect

Install Bash hook, and setup Git and Bash:

    git homemaker && git infect bash git

### Common setups

Full terminal rig:

    git infect vim nvim.config fish.config tmux clitools.config ghc extern

Darwin/Aqua rig:

    git infect yabai.config skhd.config qutebrowser

Linux/X11 rig:

    git infect x.config x qutebrowser.config qutebrowser kitty.config media.config

Doom Emacs rig:

    git infect doom-emacs
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

- [Darwin](doc.immune/darwin-setup.md)
- [Debian](doc.immune/debian-setup.md)
- [NixOS](doc.immune/nixos-setup.md)
- [Linux Kernel Development](doc.immune/linux-dev.md)
- [Key Bindings Overview](doc.immune/keybinds.md)
