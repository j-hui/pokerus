# Pokerus

Using GNU stow to sync my dotfiles


## Darwin Bootstrap

Good to have these.

#### Brew

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"

#### Key Repeat

    defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
    defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
    defaults write -g ApplePressAndHoldEnabled -bool false

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

    tree python3 htop fzf silversearcher-ag curl wget

### Linux kernel dev

Kernel build packages:

    build-essential bc bison libncurses5-dev pkg-config python libssl-dev flex

And some kernel dev aliases (this will eventually go into a .bash_linux file)

    alias dw='sudo dmesg -w'
    alias dm='sudo dmesg -c'
    alias lsgrub="grep '\$menuentry_id_option' /boot/grub/grub.cfg | sed 's/menuentry //g' | sed 's/--class.*menuentry_id_option//g' | nl -v 0"

### VirtualBox

Some nice `VBoxManage` aliases (before I figure out Vagrant):

    alias vmstart='VBoxManage startvm --type headless'
    alias vmc='VBoxManage controlvm'
    function vmpower() {
        VBoxManage controlvm $1 poweroff
    }
    function vmacpi() {
        VBoxManage controlvm $1 acpipowerbutton
    }
    alias vmls='VBoxManage list vms'
    alias vmlson='VBoxManage list runningvms'
    vmrestart() {
        vmpower $1 && vmstart $1
    }

#### Install VirtualBox Guest Additions

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

### Darwin

Good for copying keys:

    alias key='cat ~/.ssh/id_rsa.pub'

And then SSH `config`:

    Host *
        UseKeychain yes
        AddKeysToAgent yes
        ForwardAgent yes

Make Terminal.app aware of _italics_:

    curl https://gist.githubusercontent.com/sadsfae/0b4dd18670639f7dce941a1b2a9e4e9e/raw/908b48e6b6370da0568be8d138966c60240a50dd/xterm-256color-italic.terminfo > xterm-256color-italic.terminfo
    tic xterm-256color-italic.terminfo

    # add this to .bashrc
    export TERM=xterm-256color-italic
