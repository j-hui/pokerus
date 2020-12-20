{ config, pkgs, lib, ... }:
with lib;
let
  unstable = import <nixpkgs-unstable> {};
  cfg = config.pokerus.console;
in
{
  options.pokerus.console = {
    enable = mkEnableOption "Console configuration";
    mail.enable = mkEnableOption "Mail services";
    virt.enable = mkEnableOption "Virtualization services";
    print.enable = mkEnableOption "Printing services";
    laptop.enable = mkEnableOption "Laptop settings";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      console.font = "Lat2-Terminus16";
      time.timeZone = "America/New_York";
      i18n.defaultLocale = "en_US.UTF-8";

      environment.variables.EDITOR = "vim";

      environment.systemPackages = with pkgs; [
        # shell + terminal
        bash fish zsh
        sudo tmux screen
        neofetch
        thefuck
        asciinema

        # text editing
        ed nano vimHugeX neovim neovim-remote emacs

        # version control
        git gitAndTools.gh
        subversion
        mercurial

        # security
        pass
        mkpasswd

        # files
        ranger
        fzf
        ag
        ripgrep
        fd
        bat
        diskus

        # utilities
        binutils-unwrapped
        unzip
        pv

        # process management
        htop
        tree
        killall
        gotop

        # networking
        wget curl
        bind whois inetutils
        wireguard-tools
        python38Packages.speedtest-cli
        dhcp

        # disk
        parted cryptsetup gptfdisk
        btrfs-progs
        fuse
        # hardware management
        smartmontools lsof pciutils glxinfo acpi
        radeontop lshw mcelog fwts

      ];

      services = {
        openssh.enable = true;
        openssh.forwardX11 = true;
      };

      networking.wireguard.enable = true;

      programs.ssh.askPassword = "";
      programs.ssh.startAgent = true;
      programs.ssh.agentTimeout = "12h";
      programs.gnupg.agent.enable = true;
      # To adjust cache duration, add to ~/.gnpupg/gpg-agent.conf:
      #   default-cache-ttl 360

      security.sudo.configFile = ''
        Defaults timestamp_timeout=240
      '';
    })

    (mkIf cfg.laptop.enable {
      environment.systemPackages = with pkgs; [
        brightnessctl
      ];
    })

    (mkIf cfg.print.enable {
      services = {
        printing.enable = true;
        avahi.enable = true;
      };
    })

    (mkIf cfg.virt.enable {
      environment.systemPackages = with pkgs; [
        libvirt virt-manager qemu
      ];
      virtualisation.libvirtd.enable = true;
    })

    (mkIf cfg.mail.enable {
      environment.systemPackages = with pkgs; [
        aerc
        gcalcli
        astroid notmuch offlineimap msmtp
      ];
    })
  ];
}
