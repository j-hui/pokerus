{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
in
{
  config = {
    nixpkgs.config.allowUnfree = true;

    environment.variables = {
      BROWSER = "qutebrowser";
      TERMINAL = "kitty";
    };
    environment.systemPackages = with pkgs; [
      zathura
      unstable.kitty
      firefox google-chrome chromium
      unstable.qutebrowser
      gparted
      spotify spotifywm spotify-tui
      vlc
      inkscape gimp tuxpaint drawing krita
      slack discord zoom-us skypeforlinux
      mimeo
    ];
    programs.browserpass.enable = true;
  };
}
