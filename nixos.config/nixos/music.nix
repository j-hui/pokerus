{ config, pkgs, ... }:
let
  unstable = import <nixpkgs-unstable> {};
  # secrets = import /etc/nixos/secrets.nix;
in
{
  config = {
    nixpkgs.config.allowUnfree = true;

    # services.jack = {
    #   jackd.enable = true;
    #   jackd.extraOptions = [ "-dalsa" "--device" "hw:USB" ];
    #   # support ALSA only programs via ALSA JACK PCM plugin
    #   alsa.enable = false;
    #   # support ALSA only programs via loopback device (supports programs like Steam)
    #   loopback = {
    #     enable = true;
    #   };
    # };
    # systemd.user.services.pulseaudio.environment = {
    #   JACK_PROMISCUOUS_SERVER = "jackaudio";
    # };
    # boot.kernelModules = [ "snd-seq" "snd-rawmidi" ];

    hardware.pulseaudio = {
      enable = true;
      package = pkgs.pulseaudio.override { jackaudioSupport = true; };
    };

    environment.systemPackages = with pkgs; [
      pulseaudioFull
      bitwig-studio

      audacity
      supercollider
      haskellPackages.tidal
      # unstable.jack2 unstable.libjack2 unstable.qjackctl
    ];
  };
}
