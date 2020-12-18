{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {
    config = config.nixpkgs.config;
  };
in
{
  config = {
    nixpkgs.config.allowUnfree = true;

    environment.variables = {
      BROWSER = "qutebrowser";
      TERMINAL = "kitty";
    };
    environment.systemPackages = with pkgs; [
      zathura mupdf
      unstable.kitty
      firefox google-chrome chromium
      qutebrowser
      gparted
      spotify spotifywm spotify-tui
      vlc
      inkscape gimp tuxpaint drawing gthumb
      steam
      slack unstable.discord zoom-us skypeforlinux zulip
      signal-desktop signal-cli
      mattermost-desktop matterhorn
      mimeo
      pdftk
      youtube-dl
      transmission transmission-gtk
    ];
    programs.browserpass.enable = true;

    # services.mattermost = {
    #   enable = true;
    #   siteUrl = "localhost/mattermost";
    # };
    # services.matterbridge = {
    #   enable = true;
    #   configFile = "/home/j-hui/.config/matterbridge/matterbridge.toml";
    # };
  };
}
