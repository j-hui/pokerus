{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
in
{
  config = {
    programs.steam.enable = true;
    environment.systemPackages = with pkgs; [
      playonlinux
    ];
  };
}
