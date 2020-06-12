{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "snorlax";
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s29u1u5.useDHCP = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    # consoleKeyMap = "us";
    consoleUseXkbConfig = true;
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "America/New_York";

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
    fzf
    ag 
    fd bat diskus
    gnupg

    gtk3
    bspwm sxhkd
    xautolock betterlockscreen
    compton
    yabar
    rofi
    dunst libnotify

    feh
    scrot
    ranger highlight
    mpv
    asciinema

    zathura
    kitty
    firefox
    google-chrome
    bitwarden
    slack
    discord
    vlc
    zoom-us

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
  environment.variables = { EDITOR = "vim"; };
  nixpkgs.config.allowUnfree = true;

  programs.ssh.askPassword = "";

  services = {
    openssh.enable = true;
    printing.enable = true;

    xserver = {
      enable = true;
      autorun = false;
      exportConfiguration = true;

      # videoDrivers = [ "ati_unfree" ];

      layout = "us";
      xkbOptions = "ctrl:nocaps";

      # Enable touchpad support.
      libinput.enable = true;

      desktopManager.default = "none";
      windowManager.default = "bspwm";
      windowManager.bspwm.enable = true;
      displayManager.startx.enable = true;
    };
  };

  # Open ports in the firewall.
  # networking.firewall.allowedTCPPorts = [ ... ];
  # networking.firewall.allowedUDPPorts = [ ... ];
  # Or disable the firewall altogether.
  # networking.firewall.enable = false;

  sound.enable = true;
  hardware.pulseaudio.enable = true;

  users.users."j-hui" = {
    isNormalUser = true;
    extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
  };

  # This value determines the NixOS release with which your system is to be
  # compatible, in order to avoid breaking some software such as database
  # servers. You should change this only after NixOS release notes say you
  # should.
  system.stateVersion = "19.09"; # Did you read the comment?

}
