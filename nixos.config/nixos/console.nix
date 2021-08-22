{ config, pkgs, lib, ... }:
with lib;
let
  # neovim-nightly-metadata = {
  #   upattr = "neovim-unwrapped";
  #   repo_git = "https://github.com/neovim/neovim";
  #   branch = "master";
  #   rev = "28a0f6b17ddb51f605abfcd9d48b8084545d5901";
  #   sha256 = "sha256-vXoaqhbnfv34P0E2CIEMWD+0o9jUDu95Vjl33DIGxGw=";
  # };

  unstable = import <nixpkgs-unstable> {};
  cfg = config.pokerus.console;
in
{
  options.pokerus.console = {
    enable = mkEnableOption "Console configuration";
    virt.enable = mkEnableOption "Virtualization services";
    print.enable = mkEnableOption "Printing services";
    laptop.enable = mkEnableOption "Laptop settings";
  };

  config = mkMerge [
    (mkIf cfg.enable {
      # nixpkgs.overlays = [
      #   (import (builtins.fetchTarball {
      #     url = https://github.com/nix-community/neovim-nightly-overlay/archive/master.tar.gz;
      #   }))
      # ];

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
        thefuck
        asciinema
        tealdeer
        doge

        # text editing
        ed nano
        vimHugeX
        neovim
        # neovim-nightly
        neovim-remote
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

        # Basic language servers
        rnix-lsp
        nodePackages.bash-language-server
        nodePackages.vim-language-server
        nodePackages.yaml-language-server

        # version control
        git gitAndTools.gh gitAndTools.delta
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
        ag
        ripgrep
        vgrep
        fd
        bat
        diskus
        du-dust
        exa
        broot
        nnn
        borgbackup
        rclone
        sqlite
        zoxide
        tokei

        # utilities
        binutils
        unzip
        pv
        bc
        man-pages
        miller
        jq go-pup
        w3m lynx
        file
        imagemagick
        zstd
        cmark
        mdcat
        sd
        xsv
        hexyl

        # process management
        htop
        iotop
        tree
        killall
        gotop
        procs

        # networking
        wget curl
        bind whois inetutils
        wireguard-tools
        python38Packages.speedtest-cli
        dhcp rustscan
        prettyping
        gnutls
        bandwhich

        # disk
        parted cryptsetup gptfdisk
        btrfs-progs
        fuse

        # hardware management
        smartmontools lsof pciutils glxinfo acpi
        radeontop lshw mcelog fwts lm_sensors inxi

        # testing
        stress mesa-demos
      ];

      services = {
        openssh.enable = true;
        openssh.forwardX11 = true;
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
      ];
      virtualisation.libvirtd.enable = true;
      # Also remember to add user to libvirtd group
    })
  ];
}
