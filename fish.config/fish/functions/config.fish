function config --description "Edit files in ~/.config"
    if not set -q argv[1]
        echo "Usage:"
        echo "    config <config>"
        echo
        ls ~/.config/
    else if test $argv[1] = "ssh"
        $EDITOR ~/.ssh/config
    else if test $argv[1] = "bash"
        $EDITOR ~/.bash_aliases ~/.bash_local ~/.bashrc ~/.bash_profile
    else if test $argv[1] = "vim"
        $EDITOR ~/.vimrc
    else if test $argv[1] = "xmonad"
        if test -e ~/.xmonad/shell.nix
            nix-shell ~/.xmonad/shell.nix
        else
            $EDITOR ~/.xmonad/xmonad.hs
        end
    else if test -d ~/.config/$argv[1]
        $EDITOR ~/.config/$argv[1]/*
    else if test -e ~/.config/$argv[1]
        $EDITOR ~/.config/$argv[1]
    else
        echo "Did not locate .config/ entry: $argv[1]"
        echo
        ls ~/.config/
    end
end
