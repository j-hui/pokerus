{ config, pkgs, ... }:
{
  config = {
    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = with pkgs; [
      bitwig-studio
      audacity
      supercollider
      haskellPackages.tidal
    ];
  };
}
