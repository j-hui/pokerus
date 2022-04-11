.PHONY:
default:
	@echo "No default target"

xmonadctl: xmonadctl.hs shell.nix
	nix-shell --run 'ghc --make xmonadctl'
