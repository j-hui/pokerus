# Debian/Ubuntu Setup

## Common packages

    tree python3 htop fzf silversearcher-ag ripgrep curl wget gnupg

## User Management

Add regular user:

    adduser <username>

Make `sudo`:

    usermod -aG sudo <username>

Add system user:

    adduser --system --no-create-home --shell /bin/false --group --disabled-login <username>


### Text editor

To set `editor` to point to `vim`:

    sudo update-alternatives --set editor /usr/bin/vim.basic

### Disable Root SSH Login

Edit:

    /etc/ssh/sshd_config

Set:

    PermitRootLogin no

### Boot to console

Using `systemd`:

    sudo systemctl set-default multi-user.target

Alternative, configure GRUB, writing to `/etc/default/grub`:

    #GRUB_CMDLINE_LINUX_DEFAULT=<whatever>
    GRUB_CMDLINE_LINUX="text"
    GRUB_TERMINAL=console
