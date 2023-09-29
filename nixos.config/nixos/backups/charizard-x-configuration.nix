{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
  secrets = import /etc/nixos/secrets.nix;
in
{
  imports = [
      ./hardware-configuration.nix # results of hardware scan
    ];

  hardware.pulseaudio.enable = true;

  system.stateVersion = "20.03";
  nixpkgs.config.allowUnfree = true;

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  # boot.loader.grub = {
  #   enable = true;
  #   version = 2;
  #   device = "/dev/sda";
  #   # useOSProber = true;
  # };

  networking.hostName = "charizard-x";

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.useDHCP = false; # deprecated; explicitly set to false
  networking.interfaces.eno1.useDHCP = true;
  # networking.interfaces.wlp0s29u1u5.useDHCP = true;
  networking.wireguard.enable = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  i18n.defaultLocale = "en_US.UTF-8";
  i18n.inputMethod = {
    enabled = "ibus";
    ibus.engines = with pkgs.ibus-engines; [ libpinyin ];
  };
  time.timeZone = "America/New_York";

  sound.enable = true;

  users.users."j-hui" = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.variables = {
    EDITOR = "nvim";
    BROWSER = "firefox";
    TERMINAL = "kitty";
  };

  environment.systemPackages = with pkgs; [
    bash sudo
    git
    stow
    tmux

    ed
    vimHugeX neovim
    emacs
    vscode

    binutils-unwrapped
    pv
    wget curl
    unzip
    gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
    htop
    tree
    gnupg
    pass
    smartmontools
    lsof pciutils
    glxinfo

    fzf
    ag ripgrep
    fd bat diskus
    thefuck

    xorg.libX11
    gtk3
    bspwm sxhkd
    xss-lock
    xdo xdotool wmctrl xorg.xev
    xsecurelock
    yabar polybarFull
    rofi rofi-pass rofi-calc rofi-emoji rofi-systemd
    dunst libnotify
    pinentry pinentry-gtk2

    xclip
    feh
    scrot
    ranger highlight
    mpv
    asciinema
    neofetch
    libqalculate
    wordnet
    aspell aspellDicts.en scowl
    simplescreenrecorder
    gnome3.gucharmap

    zathura
    unstable.kitty
    firefox google-chrome chromium
    unstable.qutebrowser
    spotify spotifywm spotify-tui
    vlc
    slack discord zoom-us
    wireguard-tools
    # notmuch offlineimap msmtp astroid
    aerc
    w3m

    (python3.withPackages(ps: with ps; [
        virtualenvwrapper
    ]))

    stack ghc
    cargo rustfmt rustup
    tectonic texlive.combined.scheme-full
    pandoc haskellPackages.pandoc-citeproc
    opam

    bitwig-studio
    supercollider
    haskellPackages.tidal

  ];

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      vistafonts
      inconsolata
      terminus_font
      proggyfonts
      dejavu_fonts
      font-awesome
      font-awesome-ttf
      ubuntu_font_family
      source-code-pro
      source-sans-pro
      source-serif-pro
      # nerdfonts
      envypn-font
      unifont
      material-icons
      noto-fonts noto-fonts-emoji noto-fonts-extra
    ];
  };

  programs.ssh.askPassword = "";
  programs.ssh.startAgent = true;
  programs.ssh.agentTimeout = "12h";

  programs.gnupg.agent.enable = true;
  # To adjust cache duration, add to ~/.gnpupg/gpg-agent.conf:
  #   default-cache-ttl 360

  programs.browserpass.enable = true;


  services = {
    openssh.enable = true;
    openssh.forwardX11 = true;
    printing.enable = true;

    compton = {
      enable = true;                    # Application transparency
      vSync = true;                     # Remove screen tearing
      backend = "glx";
      inactiveOpacity = 0.8;          # Make programs blur on unfocus

      fade = true;
      fadeSteps = [ 0.05 0.2 ];

      shadow = true;
    };

    xserver = {
      enable = true;
      # autorun = false;
      exportConfiguration = true;

      videoDrivers = [ "amdgpu" # "ati_unfree"
                      "radeon" "cirrus" "vesa" "modesetting"];

      layout = "us";
      xkbOptions = "ctrl:nocaps";
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;

      # Enable touchpad support.
      libinput = {
        enable = true;
        accelProfile = "flat";
        tappingDragLock = false;
      };


      desktopManager = {
        xterm.enable = false;
        # xfce = {
        #   enable = true;
        #   noDesktop = true;
        #   enableXfwm = false;
        # };
      };
      windowManager.bspwm.enable = true;
      # displayManager.startx.enable = true;
      displayManager.defaultSession = "none+bspwm";
      displayManager.lightdm = {
        enable = true;
        greeters.mini = {
          enable = true;
          user = "j-hui";
          extraConfig = ''
                [greeter]
                show-password-label = true
                password-label-text = Password:
                invalid-password-text = Invalid Password
                show-input-cursor = false
                password-alignment = left

                [greeter-hotkeys]
                mod-key = meta
                shutdown-key = s
                restart-key = r
                hibernate-key = h
                suspend-key = u

                [greeter-theme]
                text-color = "#696969"
                error-color = "#fc5571"
                background-image = ""
                background-color = "#000000"
                window-color = "#000000"
                border-color = "#967b5e"
                border-width = 0px
                layout-space = 15
                password-color = "#424242"
                password-background-color = "#000000"
                password-border-color = "#000000"
                password-border-width = 0
            '';
        };
      };

      serverFlagsSection =
        ''
        Option "BlankTime" "0"
        Option "StandbyTime" "10"
        Option "SuspendTime" "20"
        Option "OffTime" "30"
        '';
    };
  };

  security.sudo = {
    configFile = ''
      Defaults timestamp_timeout=240
    '';
  };
  programs = {
    xss-lock = {
      enable = true;
      lockerCommand = ''${pkgs.xsecurelock}/bin/xsecurelock'';
      extraOptions = [''-n'' ''${pkgs.xsecurelock}/libexec/xsecurelock/dimmer''];
    };
  };
}
