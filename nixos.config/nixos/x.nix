{ config, pkgs, ... }:
{
  config = {
    nixpkgs.config.allowUnfree = true;
    console.useXkbConfig = true;

    i18n.inputMethod = {
      enabled = "ibus";
      ibus.engines = with pkgs.ibus-engines; [ libpinyin ];
    };

    sound.enable = true;
    hardware.pulseaudio.enable = true;

    environment.systemPackages = with pkgs; [
      gtk3
      bspwm sxhkd
      xss-lock
      xdo xdotool wmctrl xorg.xev
      xsecurelock
      polybarFull
      rofi rofi-pass rofi-calc rofi-emoji rofi-systemd
      dunst libnotify
      pinentry pinentry-gtk2
      pavucontrol

      xclip
      feh
      scrot
      ranger
      mpv
      simplescreenrecorder
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

    programs = {
      xss-lock = {
        enable = true;
        lockerCommand = ''${pkgs.xsecurelock}/bin/xsecurelock'';
        extraOptions = [''-n'' ''${pkgs.xsecurelock}/libexec/xsecurelock/dimmer''];
      };
    };

    services = {
      compton = {
        enable = true;
        vSync = true;
        backend = "glx";
        inactiveOpacity = 0.8;

        fade = true;
        fadeSteps = [ 0.05 0.2 ];

        shadow = true;
      };
      xserver = {
        enable = true;
        exportConfiguration = true;
        layout = "us";
        xkbOptions = "ctrl:nocaps";
        autoRepeatDelay = 269;
        autoRepeatInterval = 40;

        videoDrivers = [ "amdgpu" # "ati_unfree"
                        "radeon" "cirrus" "vesa" "modesetting"];

        # Enable touchpad support.
        libinput = {
          enable = true;
          accelProfile = "flat";
          tappingDragLock = false;
        };

        serverFlagsSection =
          ''
          Option "BlankTime" "0"
          Option "StandbyTime" "10"
          Option "SuspendTime" "20"
          Option "OffTime" "30"
          '';

        desktopManager = {
          xterm.enable = false;
          # xfce = {
          #   enable = true;
          #   noDesktop = true;
          #   enableXfwm = false;
          # };
        };

        desktopManager.wallpaper.mode = "fill";
        windowManager.bspwm.enable = true;
        displayManager.defaultSession = "none+bspwm";
        displayManager.lightdm = {
          enable = true;
          greeters.pantheon.enable = true;
          # greeters.mini = {
          #   enable = true;
          #   # user = "j-hui";
          #   extraConfig = ''
          #         [greeter]
          #         show-password-label = true
          #         password-label-text = Password:
          #         invalid-password-text = Invalid Password
          #         show-input-cursor = false
          #         password-alignment = left

          #         [greeter-hotkeys]
          #         mod-key = meta
          #         shutdown-key = s
          #         restart-key = r
          #         hibernate-key = h
          #         suspend-key = u

          #         [greeter-theme]
          #         text-color = "#696969"
          #         error-color = "#fc5571"
          #         background-image = ""
          #         background-color = "#000000"
          #         window-color = "#000000"
          #         border-color = "#967b5e"
          #         border-width = 0px
          #         layout-space = 15
          #         password-color = "#424242"
          #         password-background-color = "#000000"
          #         password-border-color = "#000000"
          #         password-border-width = 0
          #     '';
          # };
        };
      };
    };
  };
}
