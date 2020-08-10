# Darwin Setup

Good to have these.

## Install Brew

    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"


## Fast `KeyRepeat`

    defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
    defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
    defaults write -g ApplePressAndHoldEnabled -bool false

## SSH config

    Host *
        UseKeychain yes
        AddKeysToAgent yes
        ForwardAgent yes

### Other tips

Show file extensions:

    defaults write -g AppleShowAllExtensions -bool true

Finder in home directory:

    defaults read com.apple.finder NewWindowTargetPath -string file:///Users/`whoami`

TODO: Port more from https://github.com/MartinHarding/macOSuckless

Control where screenshots are sent:

    mkdir -p ~/data/screenshots
    ln -s ~/data/screenshots ~/Desktop
    defaults write com.apple.screencapture ~/data/screenshots
    killall SystemUIServer
