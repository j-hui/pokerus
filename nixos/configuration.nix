# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, pkgs, ... }:

{
  imports =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "snorlax"; # Define your hostname.
  # networking.wireless.enable = true;  # Enables wireless support via wpa_supplicant.

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.eno1.useDHCP = true;
  networking.interfaces.wlp0s29u1u5.useDHCP = true;

  # Configure network proxy if necessary
  # networking.proxy.default = "http://user:password@proxy:port/";
  # networking.proxy.noProxy = "127.0.0.1,localhost,internal.domain";

  # Select internationalisation properties.
  i18n = {
    consoleFont = "Lat2-Terminus16";
    # consoleKeyMap = "us";
    consoleUseXkbConfig = true;
    defaultLocale = "en_US.UTF-8";
  };

  # Set your time zone.
  time.timeZone = "America/New_York";

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    bash sudo
    git
    stow
    tmux

    ed
    vim neovim
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
    i3lock-color
    compton
    yabar
    rofi
    dunst libnotify

    feh
    scrot
    ranger highlight
    mpv

    zathura
    kitty
    firefox
    google-chrome
    bitwarden
    slack
    discord
    vlc

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

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = { enable = true; enableSSHSupport = true; };

  # List services that you want to enable:

  # Enable the OpenSSH daemon.

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

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  # Define a user account. Don't forget to set a password with ‘passwd’.
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
