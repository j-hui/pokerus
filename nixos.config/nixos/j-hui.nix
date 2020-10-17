{ config, pkgs, ... }:
{
  config = {
    users.users."j-hui" = {
      isNormalUser = true;
      extraGroups = [ "wheel" ];
    };
    # services.xserver.displayManager.lightdm.greeters.mini.user = "j-hui";
  };
}
