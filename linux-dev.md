# Linux Kernel Dev Setup

Assumes Ubuntu/Debian environment.

## Required packages

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

