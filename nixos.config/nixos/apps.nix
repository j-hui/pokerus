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

      programs.browserpass.enable = true;

      environment = {

        variables.BROWSER = "qutebrowser";
        variables.QUTE_BIB_FILEPATH = "/home/j-hui/Documents/qute.bib";

        systemPackages = with pkgs; [
          (unstable.qutebrowser.overrideAttrs (old: {
            propagatedBuildInputs =
              old.propagatedBuildInputs ++ [
                pkgs.python3Packages.setuptools # TODO: remove after upstream fix
              ];
          }))

          firefox google-chrome chromium

          youtube-dl
          transmission transmission-gtk
        ];
      };
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
        playerctl
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
        libreoffice
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
        carla

        # surge

        audacity
        supercollider
        haskellPackages.tidal
        # unstable.jack2 unstable.libjack2 unstable.qjackctl
      ];

      environment.variables = {
        VST_PATH = "/nix/var/nix/profiles/default/lib/vst:/var/run/current-system/sw/lib/vst:~/.vst";
        LXVST_PATH = "/nix/var/nix/profiles/default/lib/lxvst:/var/run/current-system/sw/lib/lxvst:~/.lxvst";
        LADSPA_PATH = "/nix/var/nix/profiles/default/lib/ladspa:/var/run/current-system/sw/lib/ladspa:~/.ladspa";
        LV2_PATH = "/nix/var/nix/profiles/default/lib/lv2:/var/run/current-system/sw/lib/lv2:~/.lv2";
        DSSI_PATH = "/nix/var/nix/profiles/default/lib/dssi:/var/run/current-system/sw/lib/dssi:~/.dssi";
      };
      security.pam.loginLimits = [
        { domain = "@audio"; item = "memlock"; type = "-"; value = "unlimited"; }
        { domain = "@audio"; item = "rtprio"; type = "-"; value = "99"; }
        { domain = "@audio"; item = "nofile"; type = "soft"; value = "99999"; }
        { domain = "@audio"; item = "nofile"; type = "hard"; value = "99999"; }
      ];
    })

    (mkIf cfg.dev.enable {
      environment.systemPackages = with pkgs; [
        gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
        valgrind
        ctags
        libqalculate wordnet aspell aspellDicts.en scowl

        (python3.withPackages(ps: with ps; [
          pynvim ueberzug
          virtualenvwrapper
          mypy pylint
          tox
        ]))
        ghp-import
        libxml2

        vscode

        highlight
        opam
        pre-commit
        nodePackages.write-good proselint
        stack ghc haskellPackages.haskell-language-server
        go hugo
        # cargo rustfmt
        rustup rust-analyzer
        # rustracer
        tectonic texlive.combined.scheme-full bibclean
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
#  vim: set ts=4 sw=2 tw=80 et ft=nix:
