let
  # Pinned nixpkgs, deterministic. Last updated: 4/11/22.
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/tags/21.11.tar.gz") { };
  # Rolling updates, not deterministic.
  # pkgs = import (fetchTarball("channel:nixpkgs-unstable")) {};
in
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ haskell-language-server ];
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      xmobar
    ]))
  ];
  # shellHook =
  #   ''
  #     cd ~/.xmonad
  #     ghci xmonad
  #     exit
  #   '';
}
