{ config, pkgs, ... }:

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

  networking.hostName = "snorlax";

  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  networking.useDHCP = false; # deprecated; explicitly set to false
  networking.interfaces.eno1.useDHCP = true;
  # networking.interfaces.wlp0s29u1u5.useDHCP = true;

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  console.font = "Lat2-Terminus16";
  console.useXkbConfig = true;
  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "America/New_York";

  sound.enable = true;

  users.users."j-hui" = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  environment.variables = { EDITOR = "vim"; };
  environment.systemPackages = with pkgs; [
    bash sudo
    git
    stow
    tmux

    ed
    vimHugeX neovim
    emacs
    vscode

    wget curl
    gcc gnumake
    htop
    tree
    gnupg
    smartmontools

    fzf
    ag
    fd bat diskus
    thefuck

    gtk3
    bspwm sxhkd
    xss-lock
    i3lock-fancy
    xsecurelock
    # compton
    # picom
    yabar polybarFull
    rofi
    dunst libnotify
    nomachine-client
    x2goserver
    x2goclient

    feh
    scrot
    ranger highlight
    mpv
    asciinema
    neofetch

    zathura
    kitty
    firefox
    google-chrome
    bitwarden
    spotify vlc
    slack discord zoom-us

    (python3.withPackages(ps: with ps; [
        virtualenvwrapper
    ]))

    stack
    ghc

    tectonic
    texlive.combined.scheme-full
    pandoc
    haskellPackages.pandoc-citeproc
  ];

  programs.ssh.askPassword = "";

  services = {
    openssh.enable = true;
    openssh.forwardX11 = true;
    printing.enable = true;
    # logind.extraConfig =
    #   ''
    #   IdleAction=lock
    #   '';

    compton = {
      enable = true;                    # Application transparency
      vSync = true;                     # Remove screen tearing
      backend = "glx";
      inactiveOpacity = "0.8";          # Make programs blur on unfocus

      fade = true;
      fadeSteps = [ "0.05" "0.2"];

      shadow = true;

      #opacityRules = [
      #  "100: class_g = 'kitty' && !focused"
      #  "100: class_g = 'kitty' && focused"
      #  "100: class_g = 'emacs'"
      #  "100: class_g = 'firefox'"
      #];

      # settings = {
      #   blur-background = true;
      #   blur-background-fixed = true;
      #   glx-no-stencil = true;
      #   paint-on-overlay = true; # used
      #   unredir-if-possible = false;
      #   blur-kern = "3x3box";
      #   blur-method = "kawase";
      #   blur-strength = 10;
      #   focus-exclude = [
      #     "class_g = 'Eclipse'"
      #   ];
      #   blur-background-exclude = [
      #     "name = 'Screenshot'"
      #     "class_g = 'Escrotum'"
      #   ];
      # };
    };

    xserver = {
      enable = true;
      # autorun = false;
      exportConfiguration = true;

      # videoDrivers = [ "ati_unfree" ];

      layout = "us";
      xkbOptions = "ctrl:nocaps";
      autoRepeatDelay = 200;
      autoRepeatInterval = 25;

      # Enable touchpad support.
      libinput.enable = true;

      windowManager.bspwm.enable = true;
      displayManager.defaultSession = "none+bspwm";
      # displayManager.startx.enable = true;
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

  programs = {
    xss-lock = {
      enable = true;
      lockerCommand = ''${pkgs.xsecurelock}/bin/xsecurelock'';
      extraOptions = [''-n'' ''${pkgs.xsecurelock}/libexec/xsecurelock/dimmer''];
    };
    x2goserver = {
      enable = true;
    };
  };
}
