#!/usr/bin/env bash

# My setup script for my NixOS setup, which automates the following:
#
# - Partitioning and formatting DISK with encryption, for either UEFI or BIOS boot.
#
# - Sets up btrfs subvolumes according to [darlings] recommendation.
#
# - Generates password hashes and puts them in $MOUNT/persist/secrets/.
#
# - Generates NixOS configs and instructions, prompting user to edit them accordingly.
#
# There's also nix-pokecenter.sh, which can be used to easily unmount all disks
# mounted during this script and start over.
#
# Rationale: {{{
#
# A lot of the setup here is motivated by mt-caret's article [darlings],
# which uses btrfs snapshots and NixOS hooks to disentangle per-boot configuration
# (most of what is placed under /etc/) from persistent configuration (which lives on
# through .nix configs and whatever else is stored under /persist/).
# Plus we get an encrypted root partition (but not boot partition or swap),
# and a modern filesystem, even if we don't turn the darlings setting on.
#
# I don't expect to be reinstalling my system on different machines too often,
# so there's no need for this script to be fully automated. Instead, this script
# serves to (1) help me do all the boring/tedious stuff faster, to remove some
# of the friction of setting up a new machine; (2) automate all the error-prone
# stuff, especially all the little security-related things I can forget, like
# setting permissions etc.; and (3) document my setup process, so even if
# I don't run this script on a machine, I can still reference it for what
# commands to run. I suppose (4) would be for the benefit of anyone stalking
# my dot-files and is curious about how my NixOS machines are setup, beyond
# my .nix files. I mean I also just like writing scripts.
#
# I will likely continue to work on this script a little bit (or a lot of bits)
# every time I setup a new system, including automating some of the stuff that
# will make migrating my system easier. In particular, stuff like moving keys
# and other config files over.
#
# }}}
#
# [darlings]: https://mt-caret.github.io/blog/posts/2020-06-29-optin-state.html

# Boilerplate {{{
# Exit if we encounter any anomalies executing this script.
set -e

# Pretty-print progress
prog () {
    echo "[ POKERUS // NIX ]" "$@"
}

# Try to be secure (?)
umask 022
# Boilerplate }}}

# Settings
DISK=/dev/nvme0n1
MOUNT=/mnt
ENC=enc
HOST="charizard-x"
MAIN_USER="j-hui"
UEFI=1
SWAP_SIZE=16GiB

prog "Basic sanity checks for settings..." # {{{
if [ $(id -u) -ne 0 ]; then
    prog "Please re-run as root."
    exit 1
fi

if ! [ -b "$DISK" ]; then
    prog "$DISK does not appear to exist, or isn't a block file."
    exit 1
fi

if ! [ -d "$MOUNT" ]; then
    prog "$MOUNT does not appear to exist, or isn't a directory."
    exit 1
fi

if ! which mkpasswd > /dev/null 2>/dev/null ; then
    prog "Utility mkpasswd missing, this is needed to create password hashes later on."
    exit 1
fi

prog "LGTM."
sleep 1 # }}}

prog "Partitioning disk ($DISK)..." # {{{
sleep 2

if [ "$UEFI" -eq 1 ]; then

    # GPT format
    parted $DISK -- mklabel gpt

    # UEFI boot partition
    parted $DISK -- mkpart ESP fat32 1MiB 512MiB
    parted $DISK -- set 1 esp on
    BOOT_PART="${DISK}p1"

    # Dummy BIOS boot partition
    # (not sure why we need this? but it got things working.)
    parted $DISK -- mkpart bios_grub_boot 512MiB 1GiB
    parted $DISK -- set 2 bios_grub on

    # Swap partition
    parted $DISK -- mkpart primary linux-swap 1GiB "$SWAP_SIZE"
    SWAP_PART="${DISK}p3"

    # Encrypted partition
    parted $DISK -- mkpart primary "$SWAP_SIZE" 100%
    ENC_PART="${DISK}p4"

else # BIOS (legacy) disk setup

    prog "FIXME: the following is untested (recreated from memory xD)."
    prog "Manually inspect the source, experiment a little, and validate this."
    prog "Then comment out this warning and the following exit."
    exit 1

    # MSDOS format
    parted $DISK -- mklabel msdos

    # BIOS boot partition
    parted $DISK -- mkpart primary 1MiB 512MiB
    parted $DISK -- name 1 boot
    parted $DISK -- set 1 boot on
    parted $DISK -- set 1 bios_grub on # Not actually sure if this is necessary
    BOOT_PART="${DISK}p1"

    # Swap partition
    parted $DISK -- mkpart primary linux-swap 1GiB 69GiB
    SWAP_PART="${DISK}p2"

    # Encrypted partition
    parted $DISK -- mkpart primary 69GiB 100%
    ENC_PART="${DISK}p3"
fi

prog "Finished partitioning."
parted $DISK -- print
sleep 3 # }}}

prog "Encrypting $ENC_PART" # {{{
sleep 1

cryptsetup --verify-passphrase -v luksFormat "$ENC_PART"

prog "Opening encrypted disk..."
cryptsetup open "$ENC_PART" "$ENC"
sleep 1 # }}}

prog "Formatting disks..." # {{{
sleep 2

mkfs.vfat -n boot "$BOOT_PART"

mkswap "$SWAP_PART"
swapon "$SWAP_PART"

mkfs.btrfs /dev/mapper/"$ENC"

prog "Formatted."
sleep 1 # }}}

prog "Setting btrfs subvolumes..." # {{{
sleep 2

prog "Mounting encrypted root volume (/dev/mapper/$ENC) to $MOUNT..."
mount -t btrfs /dev/mapper/"$ENC" "$MOUNT"
prog "Mounted."
sleep 1

prog "Creating subvolumes..."
btrfs subvolume create "$MOUNT"/root
btrfs subvolume create "$MOUNT"/home
btrfs subvolume create "$MOUNT"/nix
btrfs subvolume create "$MOUNT"/persist
btrfs subvolume create "$MOUNT"/log
prog "Created."
ls "$MOUNT"/
sleep 1

prog "Creating empty readonly snapshot of root subvolume, which we can rollback to on every boot..."
btrfs subvolume snapshot -r "$MOUNT"/root "$MOUNT"/root-blank
prog "Snapshotted."
sleep 1

umount "$MOUNT"
prog "Finished setting up btrfs volume."
sleep 1 # }}}

prog "Mounting subvolumes (for nixos-generate-config)..." # {{{
sleep 2

mount -o subvol=root,compress=zstd,noatime /dev/mapper/"$ENC" "$MOUNT"

mkdir "$MOUNT"/home
mount -o subvol=home,compress=zstd,noatime /dev/mapper/"$ENC" "$MOUNT"/home

mkdir "$MOUNT"/nix
mount -o subvol=nix,compress=zstd,noatime /dev/mapper/"$ENC" "$MOUNT"/nix

mkdir "$MOUNT"/persist
mount -o subvol=persist,compress=zstd,noatime /dev/mapper/"$ENC" "$MOUNT"/persist

mkdir -p "$MOUNT"/var/log
mount -o subvol=log,compress=zstd,noatime /dev/mapper/"$ENC" "$MOUNT"/var/log

prog "Also mounting boot partition ($BOOT_PART) to $MOUNT/boot..."
mkdir "$MOUNT"/boot
mount "$BOOT_PART" "$MOUNT"/boot

prog "Mounted."
mount # Displays mount information
sleep 2 # }}}

prog "Generating NixOS config (nixos-generate-config)..." # {{{
sleep 1

nixos-generate-config --root "$MOUNT"

prog "Generated config."
sleep 1 # }}}

prog "Configuring passwords..." # {{{
sleep 2

prog "Please specify root password:"
root_pw="$(mkpasswd -m sha-512)"

prog "Please specify user password:"
user_pw="$(mkpasswd -m sha-512)"

# FIXME: validate passwords here

mkdir -p "$MOUNT"/persist/secrets/

cat > "$MOUNT"/persist/secrets/default.nix <<EOF
{
  users."$MAIN_USER".hashedPassword = "$user_pw";
  users.root.hashedPassword = "$root_pw";
}
EOF

chmod 600 "$MOUNT"/persist/secrets/default.nix

prog "Dumped password hashes to $MOUNT/persist/secrets/default.nix."
sleep 2 # }}}

prog "Setting up $MOUNT/etc/nixos/configuration.nix" # {{{
sleep 1

instructions=$(mktemp)

cat > "$instructions" <<EOF
# This is an example /etc/nixos/configuration.nix.
# Tune the actual configuration according to this.

{ config, pkgs, ... }:
let
  secrets = import $MOUNT/persist/secrets/default.nix;
  # FIXME post-pokerus
  # secrets = import /persist/secrets/default.nix;
in
{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      # FIXME post-pokerus
      # ./pokerus/default.nix
    ];

  # FIXME post-pokerus
  # pokerus = {
  #   # FIXME post-darlings
  #   # darlings.enable = true;

  #   users."$MAIN_USER".hashedPassword = secrets.users."$MAIN_USER".hashedPassword;
  #   users.root.hashedPassword = secrets.users.root.hashedPassword;

  #   console = {
  #     enable = true;
  #     virt.enable = true;
  #     mail.enable = true;
  #     print.enable = true;
  #   };

  #   x = {
  #     enable = true;
  #     videoDrivers = ["amd"];
  #     user = "$MAIN_USER";
  #   };

  #   apps = {
  #     web.enable = true;
  #     messaging.enable = true;
  #     media.enable = true;
  #     office.enable = true;
  #     util.enable = true;
  #     gaming.enable = true;
  #     music.enable = true;
  #     dev.enable = true;
  #   };
  # };

  networking.hostName = "$HOST";
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;

  # Use the GRUB 2 boot loader.
  boot.loader.grub = {
    enable = true;
    # version = 2;
    device = "$DISK";
    copyKernels = true;
    efiSupport = true; # Don't include for BIOS boot
  };
  boot.loader.efi.canTouchEfiVariables = true; # Don't include for BIOS boot

  system.stateVersion = "21.05"; # Whatever this should be.

  #### BOOTSTRAP CONFIGS ####

  i18n.defaultLocale = "en_US.UTF-8";
  console.font = "Lat2-Terminus16";
  time.timeZone = "America/New_York";

  nixpkgs.config.allowUnfree = true;
  environment.systemPackages = with pkgs; [
    wget vim neovim parted cryptsetup gptfdisk btrfs-progs htop tree killall
    fzf ag ripgrep fd bat diskus
    bind whois inetutils binutils-unwrapped pv wget curl unzip
    pass git gitAndTools.gh subversion mercurial
    mkpasswd
  ];

  programs.ssh.askPassword = "";
  programs.ssh.startAgent = true;
  programs.ssh.agentTimeout = "12h";
  console.useXkbConfig = true;

  services.openssh.enable = true;
  services.printing.enable = true;
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Enable the X11 windowing system.
  services.xserver = {
    enable = true;
    layout = "us";
    xkbOptions = "ctrl:nocaps";
    desktopManager = {
      xterm.enable = false;
    };

    # Boot into console:
    displayManager.startx.enable = true;

    # Have BSPWM available:
    # windowManager.bspwm.enable = true;

    # If needed, enable a Desktop Environment:
    desktopManager.gnome.enable = true;
  };

  services.xserver.libinput.enable = true;

  users.mutableUsers = false;

  users.users."$MAIN_USER" = {
    isNormalUser = true;
    extraGroups = [ "wheel" "audio" "jackaudio" ];
    hashedPassword = secrets.users."$MAIN_USER".hashedPassword;
  };
  users.users.root.hashedPassword = secrets.users.root.hashedPassword;
}
EOF

prog "About to open up $MOUNT/etc/nixos/configuration.nix in a text editor (vim)."
prog "It will be shown alongside an example config at $instructions."
sleep 4
vim -O2 "$MOUNT"/etc/nixos/configuration.nix "$instructions"

prog "Finished setting up $MOUNT/etc/nixos/configuration.nix."
sleep 1 # }}}

prog "Setting up $MOUNT/etc/nixos/hardware-configuration.nix" # {{{

cat > "$instructions" <<EOF
hardware-configuration.nix TODOs:

(1) Make sure filesystems."/var/log" has the neededForBoot = true option,
    e.g.:

        fileSystems."/var/log" =
        { device = "/dev/disk/by-uuid/f73c53b7-ae6c-4240-89c3-511ad918edcc";
          fsType = "btrfs";
          options = [ "subvol=log" "compress=zstd" "noatime" ];
          neededForBoot = true;
        };

(2) Make sure that each subvolume will be mounted with the following options:

        options = [ "subvol=<subvolume-name>" "compress=zstd" "noatime" ];

    Make sure that subvol is set to the right name.
EOF
prog "About to open up $MOUNT/etc/nixos/hardware-configuration.nix in a text editor (vim)."
prog "It will be shown alongside some instructions at $instructions."
sleep 4
vim -O2 "$MOUNT"/etc/nixos/hardware-configuration.nix "$instructions"

prog "Finished setting up $MOUNT/etc/nixos/hardware-configuration.nix."
sleep 1 # }}}

prog "Finished setup. To install NixOS, run:"  # {{{
prog "    nixos-install --root '$MOUNT' --no-root-passwd"
prog
prog "Exiting script. Perform any manual modifications, and then install."

# nixos-install --root "$MOUNT" --no-root-passwd

# prog "Finished installing NixOS."
sleep 1 # }}}

# vim: set ts=4 sw=0 tw=80 et foldmethod=marker:
