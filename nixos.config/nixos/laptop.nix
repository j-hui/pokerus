{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
in
{
  config = {
    environment.systemPackages = with pkgs; [
      brightnessctl
    ];
  };
}

