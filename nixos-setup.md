NixOS Setup
===========

Add `unstable` channel:

    # nix-channel --add https://nixos.org/channels/nixpkgs-unstable nixpkgs-unstable
    # nix-channel --update nixpkgs-unstable

This allows us to use unstable packages in `configuration.nix`:

    let
      unstable = import <nixpkgs-unstable> {};
    in
    # ...
    environment.systemPackages = with pkgs; [
      # other packages...
      unstable.kitty
    ]


Set default browser:

    $ xdg-settings set default-web-browser org.qutebrowser.qutebrowser.desktop
