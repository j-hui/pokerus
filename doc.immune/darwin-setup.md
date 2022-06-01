# Darwin Setup

## Bootstrapping Checklist

Do this before performing initial setup.

### Set hostname

Doing this makes sure `hostname` works as expected.

The "normal" way to do this is in System Preferences > Sharing, but that doesn't set the primary hostname.

Instead:

```
hostname=<hostname>
sudo scutil --set HostName "$hostname"
sudo scutil --set LocalHostName "$hostname"
sudo scutil --set ComputerName "$hostname"
dscacheutil -flushcache
```

Then reboot.

### Install Brew

Homepage for Brew can be found at [brew.sh](https://brew.sh/).

The install snippet is:

```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

This installs `brew` to `/opt/homebrew/bin`. Add it to `PATH`:

```sh
export PATH="/opt/homebrew/bin:$PATH"
```

### Install coreutils

```
brew install coreutils
```

This ensures `realpath` is available.

### Setup SSH

Create new SSH key:

```
mkdir -p ~/.ssh &&
ssh-keygen -t ed25519 -C "email@domain.com" -f ~/.ssh/"$(hostname)"
```

Add that to GitHub.

Initialize ssh config file:

```
cat >> ~/.ssh/config << EOF
Host *
    UseKeychain yes
    AddKeysToAgent yes
    ForwardAgent yes
    IdentityFile ~/.ssh/$(hostname)
EOF
```
## System Provisioning

### Command-Line Utilities

```shell
brew install vim neovim ripgrep exa dust diskus htop bottom bat tree tmux
```

### GUI Applications

```shell
brew instasll alacritty kitty google-chrome
```

### GUI applications

### Fast `KeyRepeat`

```shell
defaults write -g KeyRepeat -int 1 # normal minimum is 2 (30 ms)
defaults write -g InitialKeyRepeat -int 10 # normal minimum is 15 (225 ms)
defaults write -g ApplePressAndHoldEnabled -bool false
```
### Show file extensions

```shell
defaults write -g AppleShowAllExtensions -bool true
```

### Finder in home directory

```shell
defaults read com.apple.finder NewWindowTargetPath -string file:///Users/`whoami`
```

### Get rid of marks display in Terminal

Select View > Hide Marks.

----

TODO: Port more from https://github.com/MartinHarding/macOSuckless

Control where screenshots are sent:

    mkdir -p ~/data/screenshots
    ln -s ~/data/screenshots ~/Desktop
    defaults write com.apple.screencapture ~/data/screenshots
    killall SystemUIServer
