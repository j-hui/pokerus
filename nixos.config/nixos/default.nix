{ config, pkgs, ... }:
let
  # unstable = import <nixpkgs-unstable> {};
  unstableTarball = fetchTarball
      https://github.com/NixOS/nixpkgs-channels/archive/nixos-unstable.tar.gz;
in
{
  require = [
    ./darlings.nix
    ./users.nix
    ./console.nix
    ./x.nix
    ./apps.nix
  ];
}
