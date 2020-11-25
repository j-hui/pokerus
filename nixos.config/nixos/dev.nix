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
      ghp-import
      libxml2

      vscode


      highlight
      opam
      pre-commit
      stack ghc
      go hugo
      cargo rustfmt rustup rust-analyzer
      tectonic texlive.combined.scheme-full
      pandoc haskellPackages.pandoc-citeproc
      elan
    ];
  };
}
