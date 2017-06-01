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

	git clone git@github.com:j-hui/pokerus.git


## Prerequisites

	vim tmux


## Usage

	./alias.sh
	./infect.sh bash git tmux vim
