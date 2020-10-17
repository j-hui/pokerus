{ config, pkgs, ... }:
{
  config = {
    nixpkgs.config.allowUnfree = true;

    environment.systemPackages = with pkgs; [
      gcc gnumake automake cmake autoconf pkg-config m4 libtool dpkg
      libqalculate wordnet aspell aspellDicts.en scowl

      (python3.withPackages(ps: with ps; [
          virtualenvwrapper
      ]))

      vscode

      highlight
      opam
      stack ghc
      cargo rustfmt rustup
      tectonic texlive.combined.scheme-full
      pandoc haskellPackages.pandoc-citeproc
    ];
  };
}
