{ config, pkgs, ... }:
{
  config = {
    users.users."j-hui" = {
      isNormalUser = true;
      extraGroups = [ "wheel" "audio" "jackaudio" ];
    };

    services.xserver.displayManager.lightdm.greeters.mini.user = "j-hui";
  };
}
