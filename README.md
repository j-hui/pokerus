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

	wget http://ftp.gnu.org/gnu/stow/stow-latest.tar.gz


## Usage

	# optional: ./disinfect.sh
	git submodule update --init --recursive
	./alias.sh
	./infect.sh bash git tmux vim

## User Management

### Disable Root SSH Login

Edit:

    /etc/ssh/sshd_config

Set:

    PermitRootLogin no

### Ubuntu

Add regular user:

    adduser <username>

Make `sudo`:

    usermod -aG sudo <username>

Add system user:

    adduser --system --no-create-home --shell /bin/false --group --disabled-login <username>

## Extras

### Setup

Git:

	git config --global user.email 
	git config --global user.name 

Debian `alternative`s:

    sudo update-alternatives --set editor /usr/bin/vim.basic

No password for `sudo` (put at bottom):

    <user> ALL=(ALL) NOPASSWD:ALL

### Common packages

Useful:
    
    tree python3 htop

Linux kernel dev:

    build-essential bc bison libncurses5-dev pkg-config python

### Install VirtualBox Guest Additions

Download ISO file:

    sudo apt-get install virtualbox-guest-additions-iso

Mount & install:

    sudo mount /usr/share/virtualbox/VBoxGuestAdditions.iso /mnt
    sudo /mnt/VBoxLinuxAdditions.run


### Make headless

Write to `/etc/default/grub`:
    
    #GRUB_CMDLINE_LINUX_DEFAULT=<whatever>
    GRUB_CMDLINE_LINUX="text"
    GRUB_TERMINAL=console

For `systemd`:

    systemctl set-default multi-user.target

