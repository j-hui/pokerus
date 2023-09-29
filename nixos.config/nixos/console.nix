{ config, pkgs, lib, ... }:
with lib;
let
  unstable = import <nixpkgs-unstable> {};
  cfg = config.pokerus.console;
in
{
  options.pokerus.console = {
    enable = mkEnableOption "Console configuration";
    virt.enable = mkEnableOption "Virtualization services";
    docker.enable = mkEnableOption "Docker services";
    print.enable = mkEnableOption "Printing services";
    laptop.enable = mkEnableOption "Laptop settings";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      console.font = "Lat2-Terminus16";
      time.timeZone = "America/New_York";
      i18n.defaultLocale = "en_US.UTF-8";

      environment.variables.EDITOR = "nvim";
      environment.variables.ESCDELAY = "10";

      environment.systemPackages = with pkgs; [
        # shell + terminal
        bash fish zsh
        sudo tmux screen
        neofetch
        asciinema
        ruby

        # text editing
        ed nano
        vimHugeX
        unstable.neovim
        unstable.neovim-remote
        unstable.tree-sitter
        # emacs
        # It was fun while it lasted
        # (wrapNeovim (neovim-unwrapped.overrideAttrs(old: {
        #   version = "0.5.0-${metadata.rev}";
        #   src = fetchFromGitHub {
        #     owner = "neovim";
        #     repo = "neovim";
        #     rev = metadata.rev;
        #     sha256 = metadata.sha256;
        #   };
        #   buildInputs = old.buildInputs ++ [ unstable.tree-sitter ];
        # })) {})

        # version control
        gitAndTools.gitFull gitAndTools.gh gitAndTools.delta
        git-subrepo
        subversion
        mercurial

        # security
        pass
        mkpasswd
        gopass
        ripasso-cursive
        xkcdpass

        # files
        ranger
        fzf
        ripgrep
        fd
        bat
        diskus
        du-dust
        exa
        tokei # count lines of code
        sd # substitution tool
        ruplacer # recursive replacement on steroids
        trash-cli

        # this its own thing
        unstable.zk

        # this is its own system
        unstable.taskwarrior
        unstable.taskwarrior-tui

        # TO REVISIT
        unstable.xplr # double check
        borgbackup # use
        rclone

        # Whatis this???
        miller
        poppler_utils # what is this used for??

        # utilities
        binutils

        imagemagick

        unzip
        zstd
        pv
        bc
        man-pages
        file
        hexyl
        p7zip
        magic-wormhole

        jq

        w3m lynx

        xsv

        cmark
        mdcat


        # nix utilities
        patchelf
        nix-index
        nix-prefetch-git

        # process management
        htop
        iotop
        tree
        killall
        gotop
        unstable.bottom
        procs

        # networking
        wget curl
        bind whois inetutils
        wireguard-tools
        python38Packages.speedtest-cli
        dhcp rustscan
        prettyping
        gnutls
        bridge-utils
        bandwhich

        # disk
        parted cryptsetup gptfdisk
        btrfs-progs
        fuse

        # hardware management
        smartmontools lsof pciutils glxinfo acpi
        radeontop lshw mcelog fwts lm_sensors inxi
        usbutils
        unstable.qmk

        # testing
        stress mesa-demos
      ];

      services = {
        openssh.enable = true;
        openssh.settings.X11Forwarding = true;
      };

      # networking.wireguard.enable = true;
      environment.etc.wireguard.source = "/persist/etc/wireguard";

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
      networking.networkmanager.enable = true;
      hardware.bluetooth.enable = true;
      hardware.logitech.wireless.enable = true;
      hardware.logitech.wireless.enableGraphical = true;
      environment.systemPackages = with pkgs; [
        brightnessctl
      ];
    })

    (mkIf cfg.print.enable {
      services = {
        printing.enable = true;
        printing.drivers = with pkgs; [
          hplipWithPlugin
          # To add printers:
          # nix run nixpkgs.hplipWithPlugin -c sudo hp-setup
        ];
        avahi.enable = true;
        avahi.nssmdns = true;
      };
    })

    (mkIf cfg.virt.enable {
      environment.systemPackages = with pkgs; [
        libvirt virt-manager qemu
        vagrant
      ];
      # For libvirt
      virtualisation.libvirtd.enable = true;
      # Also remember to add user to libvirtd group

      # For docker
      virtualisation.docker.enable = true;

      # For virtualbox
      # virtualisation.virtualbox.host.enable = true;
      # virtualisation.virtualbox.host.enableExtensionPack = true;
      # Also remember to add user to user-with-access-to-virtualbox group

      # For vagrant
      environment.variables.VAGRANT_DEFAULT_PROVIDER = "libvirt";
      services.nfs.server.enable = true;
      networking.firewall.extraCommands = ''
        ip46tables -I INPUT 1 -i vboxnet+ -p tcp -m tcp --dport 2049 -j ACCEPT
      '';
    })

    (mkIf cfg.docker.enable {
      environment.systemPackages = with pkgs; [
        docker
      ];
      virtualisation.docker.enable = true;
    })
  ];
}
