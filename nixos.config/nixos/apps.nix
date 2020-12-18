{ config, pkgs, lib, ... }:
with lib;
let
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
  cfg = config.pokerus.apps;
in
{

  options.pokerus.apps = {
    web.enable = mkEnableOption "Web browsing";
    messaging.enable = mkEnableOption "Messaging applications";
    media.enable = mkEnableOption "Multimedia applications";
    office.enable = mkEnableOption "Office applications";
    util.enable = mkEnableOption "Utilities";
    gaming.enable = mkEnableOption "Games";
    music.enable = mkEnableOption "Music production";
    dev.enable = mkEnableOption "Software development";
  };

  config = mkMerge [
    { nixpkgs.config.allowUnfree = true; }

    (mkIf cfg.web.enable {
      environment = {
        variables.BROWSER = "qutebrowser";
        systemPackages = with pkgs; [
          firefox google-chrome chromium
          qutebrowser
          youtube-dl
          transmission transmission-gtk
        ];
      };
      programs.browserpass.enable = true;
    })

    (mkIf cfg.messaging.enable {
      environment.systemPackages = with pkgs; [
        slack
        unstable.discord
        zoom-us
        skypeforlinux
        zulip
        signal-desktop signal-cli
        mattermost-desktop matterhorn
      ];
    })

    (mkIf cfg.media.enable {
      environment.systemPackages = with pkgs; [
        spotify spotifywm spotify-tui spotifyd spotify-tui
        feh
        mpv
        vlc
      ];
    })

    (mkIf cfg.office.enable {
      environment.systemPackages = with pkgs; [
        inkscape gimp tuxpaint drawing gthumb
        zathura mupdf
        pdftk
      ];
    })

    (mkIf cfg.util.enable {
      environment = {
        variables.TERMINAL = "kitty";
        systemPackages = with pkgs; [
          unstable.kitty
          gparted
        ];
      };
    })

    (mkIf cfg.gaming.enable {
      environment.systemPackages = with pkgs; [
        steam
        playonlinux
      ];
      programs.steam.enable = true;
    })

    (mkIf cfg.music.enable {
      environment.systemPackages = with pkgs; [
        pulseaudioFull
        bitwig-studio

        audacity
        supercollider
        haskellPackages.tidal
        # unstable.jack2 unstable.libjack2 unstable.qjackctl
      ];
    })

    (mkIf cfg.music.enable {
      environment.systemPackages = with pkgs; [
        gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
        libqalculate wordnet aspell aspellDicts.en scowl

        (python3.withPackages(ps: with ps; [
            virtualenvwrapper
        ]))
        ghp-import
        libxml2

        vscode


        highlight
        opam
        pre-commit
        stack ghc
        go hugo
        # cargo rustfmt
        rustup  # rust-analyzer
        tectonic texlive.combined.scheme-full
        pandoc haskellPackages.pandoc-citeproc
        elan
      ];
    })
  ];

    # services.spotifyd = {
      #   enable = true;
      #   config = ''
      #     [global]
      #     username = j-hui
      #     password = pliable-pucker-apply6

      #     backend = pulseaudio

      #     device_name = charizard-x
      #     '';
      # };

    # services.mattermost = {
    #   enable = true;
    #   siteUrl = "localhost/mattermost";
    # };
    # services.matterbridge = {
    #   enable = true;
    #   configFile = "/home/j-hui/.config/matterbridge/matterbridge.toml";
    # };
}
