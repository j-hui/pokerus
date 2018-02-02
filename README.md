# Pokerus

Using GNU stow to sync my dotfiles


## Darwin Bootstrap

Good to have these.

#### Brew

	/usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#### Key Repeat

	defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
	defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
	defaults write -g ApplePressAndHoldEnabled -bool true

## Download

	git clone git@github.com:j-hui/pokerus.git ~/pokerus
	cd ~/pokerus


## Prerequisites

	sudo git stow vim tmux make

If GNU stow is unavailable, get it from the GNU mirror directly:

	wget http://mirrors.peers.community/mirrors/gnu/stow/stow-latest.tar.gz


## Usage

	# optional: ./disinfect.sh
	git submodule update --init --recursive
	./alias.sh
	./infect.sh bash git tmux vim

## Extras

Set up git:

	git config --global user.email 
	git config --global user.name 


Linux kernel dev:

    build-essential bc bison libncurses5-dev pkg-config python


## Make headless

Write to `/etc/default/grub`:
    
    #GRUB_CMDLINE_LINUX_DEFAULT=<whatever>
    GRUB_CMDLINE_LINUX="text"
    GRUB_TERMINAL=console

For `systemd`:

    systemctl set-default multi-user.target

