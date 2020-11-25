{ config, pkgs, ... }:
let
  secrets = import /persist/secrets/default.nix;
in
{
  config = {
    users = {
      mutableUsers = false;

      users."j-hui" = {
        isNormalUser = true;
        extraGroups = [ "wheel" "audio" "jackaudio" ];
        hashedPassword = secrets.users."j-hui".hashedPassword;
      };
    };

    environment.systemPackages = with pkgs; [
      mkpasswd
    ];

    services.xserver.displayManager.lightdm.greeters.mini.user = "j-hui";
  };
}
