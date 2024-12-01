# Pokerus

Maintaining my dot files across multiple environments.

## Basic setup

Ensure these core dependencies are installed:

    sudo git bash

If bootstrapping macOS, see [instructions](doc.immune/darwin-setup.md).
If bootstrapping Debian, see [instructions](doc.immune/debian-setup.md).

Then, retrieve this repo and set up git aliases:

    git clone git@github.com:j-hui/pokerus.git ~/pokerus && cd ~/pokerus && git config --local include.path ../.gitconfig

Install Bash hook, and setup Git and Bash:

    git homemaker && git infect bash git

### Common setups

Full terminal rig:

    git infect vim nvim.config zsh.config tmux clitools.config tms.local

Darwin/Aqua rig:

    git infect yabai.config skhd.config qutebrowser

Linux/X11 rig:

    git infect x.config x qutebrowser.config qutebrowser alacritty.config media.config desktop.local-share

Doom Emacs rig:

    git infect doom-emacs
    git clone --depth 1 https://github.com/hlissner/doom-emacs ~/.emacs.d && \
        ~/.emacs.d/bin/doom install

If `install` fails, running it a second term fixes it for some reason..

## Additional setup

### Git user

    git config --global user.email <email>
    git config --global user.name <name>

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
