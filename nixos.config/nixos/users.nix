{ config, pkgs, lib, ... }:
with lib;
let
  cfg = config.pokerus.users;
in
{
  options.pokerus.users = {
    fish.enable = mkEnableOption "Use fish shell";
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
          extraGroups = [ "wheel" "audio" "jackaudio" "networkmanager" "libvirtd" "docker" "user-with-access-to-virtualbox" ];
        };
      };
    }

    (mkIf cfg.fish.enable {
      users.users."j-hui".shell = pkgs.fish;
      programs.fish.enable = true;
    })
  ];
}
