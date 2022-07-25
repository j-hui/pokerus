{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.pokerus.users;
in
{
  options.pokerus.users = {
    "j-hui".hashedPassword = mkOption { type = types.str; };
    root.hashedPassword = mkOption { type = types.str; };
  };

  config = mkMerge [
    {
      users = {
        mutableUsers = false;

        users.root.hashedPassword = cfg.root.hashedPassword;

        users."j-hui" = {
          isNormalUser = true;
          hashedPassword = cfg."j-hui".hashedPassword;
          extraGroups = [ "wheel" "audio" "jackaudio" "networkmanager" "libvirtd" "docker" "user-with-access-to-virtualbox" "lpadmin" ];
          shell = pkgs.zsh;
        };
      };
      programs.zsh.enable = true;
    }
  ];
}
