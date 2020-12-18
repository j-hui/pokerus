{ config, pkgs, lib, ... }:
with lib;
let
  # secrets = import /persist/secrets/default.nix;
  cfg = config.pokerus.users;
in
{
  options.pokerus.users = {
    "j-hui".hashedPassword = mkOption { type = types.str; };
    root.hashedPassword = mkOption { type = types.str; };
  };

  config = {
    users = {
      mutableUsers = false;

      users.root.hashedPassword = cfg.root.hashedPassword;

      users."j-hui" = {
        isNormalUser = true;
        extraGroups = [ "wheel" "audio" "jackaudio" "networkmanager" ];
        hashedPassword = cfg."j-hui".hashedPassword;
          # secrets.users."j-hui".hashedPassword;
      };
    };

    # environment.systemPackages = with pkgs; [
    #   mkpasswd
    # ];

    # services.xserver.displayManager.lightdm.greeters.mini.user = "j-hui";
  };
}
