let
  # Pinned nixpkgs, deterministic. Last updated: 2/12/21.
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/a58a0b5098f0c2a389ee70eb69422a052982d990.tar.gz")) {};

  # Rolling updates, not deterministic.
  # pkgs = import (fetchTarball("channel:nixpkgs-unstable")) {};
in pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ haskell-language-server ];
  buildInputs = with pkgs; [
    (haskellPackages.ghcWithPackages (ps: with ps; [
      xmonad xmonad-contrib xmonad-extras
      dbus monad-logger
    ]))
  ];
  shellHook =
    ''
      cd ~/.xmonad
      ghci xmonad
      exit
    '';
}
