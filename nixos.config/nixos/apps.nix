{ config, pkgs, lib, ... }:
with lib;
let
  unstable = import <nixpkgs-unstable> { config.allowUnfree = true; };
  cfg = config.pokerus.apps;
  spotifydConf = pkgs.writeText "spotifyd.conf" ''
      [global]
      device_name = ${cfg.media.spotify.device-name}
      username = ${cfg.media.spotify.username}
      password = ${cfg.media.spotify.password}
      backend = pulseaudio
      use_mpris = true
      '';

  spotifydFull = (pkgs.spotifyd.override ({
          withMpris = true;
          withALSA = true;
          withPulseAudio = true;
        }));
in
{

  options.pokerus.apps = {
    web.enable = mkEnableOption "Web browsing";
    messaging.enable = mkEnableOption "Messaging applications";
    media.enable = mkEnableOption "Multimedia applications";
    media.spotify.enable = mkEnableOption "spotifyd + spotify-tui";
    media.spotify.device-name = mkOption { type = types.str; default = "spotifyd"; };
    media.spotify.username = mkOption { type = types.str; };
    media.spotify.password = mkOption { type = types.str; };
    office.enable = mkEnableOption "Office applications";
    util.enable = mkEnableOption "Utilities";
    mail.enable = mkEnableOption "Mail services";
    emacs.enable = mkEnableOption "Make (gcc)Emacs available as a service";
    emacs.gcc.enable = mkEnableOption "Use gccEmacs";
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
                pkgs.python3Packages.pocket
                # pkgs.python3Packages.setuptools # TODO: remove after upstream fix
              ];
          }))

          (unstable.luakit.overrideAttrs (old: {
          }))
          surf
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
        spotifywm
        feh
        mpv
        vlc
        android-file-transfer
        cava
        # ncmpcpp vimpc mpc_cli
      ];
    })

    (mkIf cfg.media.spotify.enable {
      environment.systemPackages = with pkgs; [
        unstable.spotify-tui
        spotifydFull
      ];

      # Upstream, spotifyd is declared a system service, which does not have
      # permissions for pulseaudio. Here we take from this PR:
      #   https://github.com/NixOS/nixpkgs/pull/77853/files#diff-45637768f3e660b4706792163812c61832c39dcbb5fce21e21e3b42e64f4fe43
      systemd.user.services.spotifyd = {
        wantedBy = [ "multi-user.target" "default.target" ];
        after = [ "network-online.target" "sound.target" ];
        description = "spotifyd, a Spotify playing daemon";
        serviceConfig = {
          ExecStart = "${spotifydFull}/bin/spotifyd --no-daemon --config-path ${spotifydConf}";
          Restart = "always";
          RestartSec = 12;
        };
      };
    })

    (mkIf cfg.office.enable {
      environment.systemPackages = with pkgs; [
        inkscape gimp tuxpaint drawing gthumb
        zathura mupdf evince
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
          termpdfpy
        ];
      };
    })

    (mkIf cfg.mail.enable {
      environment.systemPackages = with pkgs; [
        unstable.aerc
        gcalcli
        astroid
        alot
        notmuch
        lieer
        afew
        msmtp
        offlineimap
        thunderbird
        neomutt notmuch-bower unstable.meli
        cmark w3m
        newsboat
      ];
    })

    (mkIf cfg.emacs.enable  {
      environment.systemPackages = with pkgs; [
        pinentry_emacs
      ];
      services.emacs.enable = true;
    })

    (mkIf cfg.emacs.gcc.enable {
      nixpkgs.overlays = [
        (import (builtins.fetchTarball {
          url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
        }))
      ];
      services.emacs.package = pkgs.emacsGcc;
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

      systemd.tmpfiles.rules = [
        "L /usr/share/dict/words - - - - ${config.system.path}/lib/aspell/en-common.wl"
      ];
      environment.systemPackages = with pkgs; [
        vscode # I don't actually user this but handy to keep around
        editorconfig-core-c
        pre-commit

        # Writing
        libqalculate wordnet scowl
        (aspellWithDicts (ds: with ds; [
          en en-computers en-science
        ]))
        languagetool
        proselint

        # Markup
        hugo
        ghp-import
        highlight
        mdl
        scdoc

        libxml2 # not sure why this was needed?

        # random system dependencies..?
        openssl
        # dbus
        sqlite

        # C
        gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
        clang clang-tools
        cdecl
        valgrind
        unstable.universal-ctags
        ccls

        # Python
        (python3.withPackages(ps: with ps; [
          pynvim
          virtualenvwrapper
          mypy pylint
          tox
        ]))

        # Javascript
        nodePackages.javascript-typescript-langserver

        # OCaml
        opam

        # Haskell
        stack ghc haskellPackages.haskell-language-server stylish-haskell stylish-cabal

        # Go
        go gopls

        # Rust
        rustup rustfmt rust-analyzer

        # Latex
        tectonic texlive.combined.scheme-full bibclean texlab

        # Pandoc
        pandoc haskellPackages.pandoc-citeproc

        # Lean
        elan

        # Lua
        lua

        # TLA
        tlaps

        # Zig
        zig
      ];
    })
  ];


    # services.mattermost = {
    #   enable = true;
    #   siteUrl = "localhost/mattermost";
    # };
    # services.matterbridge = {
    #   enable = true;
    #   configFile = "/home/j-hui/.config/matterbridge/matterbridge.toml";
    # };

    # Mopidy + mpd is a cool idea but has too many moving parts for me to want to
    # invest more time in. It doesn't seem to work well with Spotify without
    # a lot of extra effort, and the clients aren't much better than Spotify's
    # own official desktop app. Maybe I'll revisit this some other day/year.
    # hardware.pulseaudio.tcp = { # Enable TCP for mopidy to connect to
    #   enable = true;
    #   anonymousClients.allowedIpRanges = ["127.0.0.1"];
    # };
    #
    # services.mopidy = {
    #   enable = true;
    #   extensionPackages = with pkgs; [ mopidy-spotify mopidy-mpd mopidy-iris ];
    #   configuration = ''
    #       [spotify]
    #       enabled = true
    #       client_id =
    #       client_secret =
    #       username =
    #       password =
    #       bitrate=320
    #       [mpd]
    #       enabled = true
    #       hostname = ::
    #       [http]
    #       enabled = true
    #       hostname = 127.0.0.1
    #       port = 6680
    #       default_app = iris
    #       [audio]
    #       output = pulsesink server=127.0.0.1
    #   '';
    # };
}
# vim: set ts=4 sw=2 tw=80 et ft=nix:
