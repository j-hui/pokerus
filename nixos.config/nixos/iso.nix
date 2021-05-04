# iso.nix

# TODO: This is a work in progress. I want to be able to build a custom nix ISO
# specialized for deploying my installation on new machines. This merges together
# the Live USB instructions from https://mt-caret.github.io/blog/posts/2020-06-29-optin-state.html
# and this repo I found: https://github.com/techhazard/nixos-iso. There are also
# official instructions here: https://nixos.wiki/wiki/Creating_a_NixOS_live_CD.
#
# I should be able to use this to build a live USB image with the following commands:
#
# nix-build '<nixpkgs/nixos>' -A config.system.build.isoImage -I nixos-config=iso.nix
# dd if=./result/iso/nixos-20.03....-x86_64-linux.iso of=/dev/<usb device> bs=1M status=progress

{ config, pkgs, ... }:
{
  imports = [
    # installation-cd-graphical-plasma5-new-kernel.nix uses pkgs.linuxPackages_latest
    # instead of the default kernel.
    <nixpkgs/nixos/modules/installer/cd-dvd/installation-cd-graphical-plasma5-new-kernel.nix>
    <nixpkgs/nixos/modules/installer/cd-dvd/channel.nix>
  ];

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  environment.systemPackages = with pkgs; [
    wget
    vim neovim
    git
    tmux
    gparted
    nix-prefetch-scripts
  ];

  environment.etc = {
    "nix-pokerus/nix-infect.sh" = {
      source = ./scripts/nix-infect.sh;
      mode = "0700";
    };
  };

  environment.variables = { PATH = [ "/bin" "/usr/bin" "/etc/nix-pokerus/" ]; };
}
