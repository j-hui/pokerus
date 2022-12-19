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

  spotifydFull = pkgs.spotifyd.override {
          withMpris = true;
          withALSA = true;
          withPulseAudio = true;
        };

  efm-langserver = pkgs.buildGoModule rec {
    pname = "efm-langserver";
    version = "0.0.29";

    src = pkgs.fetchFromGitHub {
      owner = "mattn";
      repo = "efm-langserver";
      rev = "v${version}";
      sha256 = "0zz63w5g8siaip0fqc5mqx7y6kcvi9c2k2w6xz8wrl3kicbwcvgw";
    };

    # subPackages = [ "." ];

    vendorSha256 = "b5c6bed5246b172bd4f2db4799f3058865ddd40f2b4121169b531cdaaa7411f2";

    meta = with lib; {
      description = "General purpose Language Server";
      homepage = "https://github.com/mattn/efm-langserver";
      license = licenses.mit;
      platforms = platforms.unix;
    };
  };
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

          unstable.qutebrowser
          unstable.nyxt
          unstable.vieb

          (unstable.luakit.overrideAttrs (old: { }))
          surf
          firefox
          unstable.google-chrome chromium

          youtube-dl
          transmission transmission-gtk
        ];
      };
    })

    (mkIf cfg.messaging.enable {
      environment.systemPackages = with pkgs; [
        slack
        unstable.discord
        unstable.zoom-us
        skypeforlinux
        zulip
        unstable.signal-desktop unstable.signal-cli
        mattermost-desktop matterhorn
      ];
    })

    (mkIf cfg.media.enable {
      environment.systemPackages = with pkgs; [
        playerctl
        spotify
        # spotifywm (makeDesktopItem {
        #   name = "Spotifywm";
        #   exec = "spotifywm %U";
        #   icon = "spotify-client";
        #   desktopName = "Spotifywm";
        #   genericName = "Music Player";
        #   categories = "Audio;Music;Player;AudioVideo;";
        #   mimeType = "x-scheme-handler/spotify;";
        #   extraEntries =
        #     ''
        #     TryExec = spotifywm
        #     StartupWMClass = spotify
        #     '';
        # })
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
        pdftk pdfgrep
        libreoffice
        enscript
      ];
    })

    (mkIf cfg.util.enable {
      environment = {
        variables.TERMINAL = "alacritty";
        variables.WINIT_X11_SCALE_FACTOR = "1";
        systemPackages = with pkgs; [
          unstable.kitty
          gparted
          termpdfpy
          alacritty
        ];
      };
    })

    (mkIf cfg.mail.enable {
      environment.systemPackages = with pkgs; [
        unstable.aerc
        dante
        neomutt

        notmuch notmuch-addrlookup afew
        unstable.lieer

        cmark w3m

        newsboat
        gcalcli
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
          url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
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

      # required for rr
      boot.kernel.sysctl."kernel.perf_event_paranoid" = 1;
      systemd.tmpfiles.rules = [
        "L /usr/share/dict/words - - - - ${config.system.path}/lib/aspell/en-common.wl"
      ];

      services.udev.packages = [  unstable.pkgs.saleae-logic-2 ];

      environment.systemPackages = with pkgs; [
        vscode # I don't actually user this but handy to keep around
        editorconfig-core-c
        pre-commit
        nodePackages.prettier

        # Debugging
        gdb
        rr # requires sysctl: kernel.perf_event_paranoid = 1
        asmfmt

        # Nix
        rnix-lsp
        unstable.statix


        # Writing
        libqalculate wordnet scowl
        (aspellWithDicts (ds: with ds; [
          en en-computers en-science
        ]))
        languagetool
        proselint
        vale

        unstable.pkgs.saleae-logic-2
        efm-langserver

        # Markup
        hugo
        ghp-import
        highlight
        mdl
        scdoc

        libxml2 # not sure why this was needed?

        nodePackages.yaml-language-server

        # random system dependencies..?
        openssl
        # dbus
        sqlite

        # C
        gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
        gcc-unwrapped gcovr
        gcc-arm-embedded
        doxygen
        clang clang-tools
        bear python3Packages.compiledb
        cdecl
        valgrind
        unstable.universal-ctags
        unstable.checkmake
        ccls

        # Python
        (python3.withPackages(ps: with ps; [
          click
          pynvim
          virtualenvwrapper
          mypy pylint
          tox
          yapf
          autopep8
        ]))
        unstable.black
        unstable.nodePackages.pyright

        # Scala
        scala
        sbt metals

        # Java
        jdk

        # Javascript
        nodejs
        yarn
        # nodePackages.javascript-typescript-langserver
        nodePackages.typescript-language-server

        # OCaml
        opam
        ocamlPackages.ocaml-lsp
        ocamlformat

        # Haskell
        unstable.stack
        ghc
        haskellPackages.hoogle
        cabal2nix cabal-install
        unstable.haskellPackages.haskell-language-server
        hlint
        stylish-haskell
        haskellPackages.floskell

        # Go
        go gopls

        # Rust
        rustup rustfmt unstable.rust-analyzer crate2nix

        # Latex
        tectonic texlive.combined.scheme-full bibclean texlab
        graphviz

        # Pandoc
        pandoc

        # Lean
        elan

        # Lua
        lua
        unstable.sumneko-lua-language-server unstable.stylua

        # TLA
        tlaps

        # Zig
        zig
        unstable.zls

        # Vim
        vim-vint
        nodePackages.vim-language-server

        # Shell
        nodePackages.bash-language-server
        shellcheck
        shfmt
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
