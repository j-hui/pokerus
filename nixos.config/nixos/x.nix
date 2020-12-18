{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.pokerus.x;
  unstable = import <nixpkgs-unstable> { nixpkgs.config.allowUnfree = true; };
in
{
  options.pokerus.x = {
    enable = mkEnableOption "Graphical X";

    ibus.enable = mkEnableOption "Input";

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
      # "ctrl:nocaps,altwin:swap_alt_win";
      default = "ctrl:nocaps";
    };
  };

  config = mkIf cfg.enable {
    nixpkgs.config.allowUnfree = true;
    console.useXkbConfig = true;

    i18n.inputMethod = mkIf cfg.ibus.enable {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ libpinyin ];
    };

    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [
      gtk3 glib unstable.dracula-theme
      bspwm sxhkd
      xss-lock
      xdo xdotool wmctrl xorg.xev
      xsecurelock
      polybarFull
      rofi-pass
      (rofi.override { plugins = [ rofi-file-browser rofi-emoji rofi-systemd rofi-calc ]; })
      dunst libnotify
      pinentry pinentry-gtk2
      pavucontrol
      xclip
      scrot
      simplescreenrecorder
      paper-gtk-theme
      paper-icon-theme
      mimeo
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
      ];
    };

    programs = {
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
          [ "radeon" "cirrus" "vesa" "modesetting"] ++ cfg.videoDrivers;

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
          # greeters.pantheon.enable = true;
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
        fadeDelta = 5;
      };

      greenclip.enable = true;
    };
  };
}
