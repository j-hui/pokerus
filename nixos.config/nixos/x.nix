{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.pokerus.x;
  unstable = import <nixpkgs-unstable> { nixpkgs.config.allowUnfree = true; };
in
{
  options.pokerus.x = {
    enable = mkEnableOption "Graphical X";

    ibus.enable = mkEnableOption "Character input engine";

    videoDrivers = mkOption {
      type = types.listOf types.str;
      default = [];
    };

    user = mkOption {
      type = types.str;
      default = "j-hui";
    };

    xkbOptions = mkOption {
      type = types.str;
      default = "ctrl:nocaps";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;
    console.useXkbConfig = true;
    hardware.pulseaudio.enable = true;

    i18n.inputMethod = mkIf cfg.ibus.enable {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ libpinyin ];
    };

    environment.systemPackages = with pkgs; [
      gtk3 glib
      lxappearance
      bspwm sxhkd
      xdo xdotool wmctrl xorg.xev
      xss-lock xsecurelock
      polybarFull
      (rofi.override { plugins = [ rofi-file-browser rofi-emoji rofi-systemd rofi-calc ]; })
      rofi-pass
      rofi-mpd
      conky
      dunst libnotify
      pinentry pinentry-gtk2
      xclip
      scrot
      pavucontrol
      mimeo
      simplescreenrecorder
      paper-gtk-theme paper-icon-theme
      unstable.dracula-theme
      xfce.thunar xfce.xfconf xfce.tumbler xfce.exo
      dragon-drop
      unstable.devour # only added to nixpkgs in nov 2020
      tabbed
      xterm st
      gnome3.gucharmap # for selecting fonts
    ];

    fonts = {
      enableFontDir = true;
      enableGhostscriptFonts = true;
      fonts = with pkgs; [
        corefonts
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
        symbola
        noto-fonts-cjk
        envypn-font
        unifont
        material-icons
        noto-fonts noto-fonts-emoji noto-fonts-extra
        siji
        emacs-all-the-icons-fonts
      ];
    };

    environment.sessionVariables = {
        XCURSOR_PATH = [
          "${config.system.path}/share/icons"
          "$HOME/.icons"
          "$HOME/.nix-profile/share/icons/"
        ];
        GTK_DATA_PREFIX = [
          "${config.system.path}"
        ];
    };

    programs = {
      dconf.enable = true;
      xss-lock = {
        enable = true;
        lockerCommand = ''${pkgs.xsecurelock}/bin/xsecurelock'';
        extraOptions = [''-n'' ''${pkgs.xsecurelock}/libexec/xsecurelock/dimmer''];
      };
    };

    services = {
      xserver = {
        enable = true;
        exportConfiguration = true;
        layout = "us";
        xkbOptions = cfg.xkbOptions;

        videoDrivers =
          cfg.videoDrivers ++ [ "radeon" "cirrus" "vesa" "modesetting"];

        # Enable touchpad support.
        libinput = {
          enable = true;
          accelProfile = "flat";
          tappingDragLock = false;
        };

        # NOTE: no difference between these modes for LED monitors
        serverFlagsSection =
          ''
          Option "BlankTime" "0"
          Option "StandbyTime" "0"
          Option "SuspendTime" "30"
          Option "OffTime" "0"
          '';

        # These aren't working
        autoRepeatDelay = 250;
        autoRepeatInterval = 69;

        desktopManager = {
          xterm.enable = false;
          wallpaper.mode = "fill";
          # xfce = {
          #   enable = true;
          #   noDesktop = true;
          #   enableXfwm = false;
          # };
        };

        windowManager.bspwm.enable = true;
        displayManager.defaultSession = "none+bspwm";
        displayManager.lightdm = {
          enable = true;
          greeters.mini = {
            enable = true;
            user = cfg.user;
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
      };

      compton = {
        enable = true;
        vSync = true;
        backend = "glx";
        inactiveOpacity = 0.96;

        fade = true;
        fadeSteps = [ 0.05 0.2 ];

        # blur-background-exclude = [
        #   "window_type = 'dock'"
        #   "window_type = 'desktop'"
        #   "class_g = 'Rofi'"
        #   "_GTK_FRAME_EXTENTS@:c"
        # ];

        # For picom only:
        # # Unredirect all windows if a full-screen opaque window is detected, to
        # # maximize performance for full-screen windows. Known to cause
        # # flickering when redirecting/unredirecting windows.
        # unredir-if-possible = true;

        # # GLX backend: Avoid using stencil buffer, useful if you don't have a
        # # stencil buffer. Might cause incorrect opacity when rendering
        # # transparent content (but never practically happened) and may not work
        # # with blur-background. Recommended.
        # glx-no-stencil = true;

        # # Use X Sync fence to sync clients' draw calls, to make sure all draw
        # # calls are finished before picom starts drawing. Needed on
        # # nvidia-drivers with GLX backend for some users.
        # xrender-sync-fence = true;

        opacityRules = [
          # "100:class_g = 'Firefox'"
          "100:class_g = 'qutebrowser'"
          # "100:class_g = 'VirtualBox Machine'"
          # Art/image programs where we need fidelity
          "100:class_g = 'Gimp'"
          "100:class_g = 'Inkscape'"
          "100:class_g = 'aseprite'"
          "100:class_g = 'krita'"
          "100:class_g = 'feh'"
          "100:class_g = 'mpv'"
          "100:class_g = 'Rofi'"
          "100:class_g = 'Peek'"
          "99:_NET_WM_STATE@:32a = '_NET_WM_STATE_FULLSCREEN'"
        ];
      };

      greenclip.enable = true;
    };
  };
}
