{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
in
{
  config = {
    # nixpkgs.config.allowUnfree = true;

    console.font = "Lat2-Terminus16";
    networking.wireguard.enable = true;
    time.timeZone = "America/New_York";
    i18n.defaultLocale = "en_US.UTF-8";

    environment.variables = {
      EDITOR = "vim";
    };

    environment.systemPackages = with pkgs; [
      bash fish zsh
      sudo git tmux screen
      ed nano vimHugeX neovim neovim-remote emacs
      binutils-unwrapped pv wget curl unzip
      bind whois inetutils
      parted cryptsetup gptfdisk
      btrfs-progs
      htop tree
      killall
      gotop
      fzf ag ripgrep fd bat diskus page
      smartmontools lsof pciutils glxinfo acpi
      wireguard-tools fuse
      neofetch
      pass
      thefuck
      asciinema
      aerc
      gcalcli
      astroid notmuch offlineimap msmtp
      libvirt virt-manager qemu
      gitAndTools.gh subversion mercurial
      dhcp
      radeontop lshw mcelog
      fwts
      python38Packages.speedtest-cli
    ];

    virtualisation.libvirtd.enable = true;

    services = {
      openssh.enable = true;
      openssh.forwardX11 = true;
      printing.enable = true;
      avahi.enable = true;
    };

    programs.ssh.askPassword = "";
    programs.ssh.startAgent = true;
    programs.ssh.agentTimeout = "12h";
    programs.gnupg.agent.enable = true;
    # To adjust cache duration, add to ~/.gnpupg/gpg-agent.conf:
    #   default-cache-ttl 360

    security.sudo = {
      configFile = ''
        Defaults timestamp_timeout=240
      '';
    };
  };
}
