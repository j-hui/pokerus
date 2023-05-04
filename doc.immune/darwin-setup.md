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

This ensures the GNU implementation of `realpath` is available (which `git infect` depends on).

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

### Install System Utilities

```shell
brew install gnupg pinentry-mac
```

### Command-Line Utilities

macOS seems to use `~/.bash_profile` instead of `~/.bashrc`. Insist on the latter:

```shell
ln -s ~/.bashrc ~/.bash_profile
```

```shell
brew install vim neovim ripgrep exa dust diskus htop bottom bat tree tmux gh node gnu-sed
```

### Patched Fonts

Install [Nerd Fonts](https://github.com/ryanoasis/nerd-fonts#option-3-install-script) via Homebrew:

```
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font font-sauce-code-pro-nerd-font font-go-mono-nerd-font font-roboto-mono-nerd-font
```

### GUI Applications

Shells:

```shell
brew install alacritty wezterm kitty warp
```

Browsers and password managers:

```shell
brew install firefox google-chrome 1password browserpass tunnelblick
```

Customization:

```shell
brew install rectangle keycastr alt-tab
```

Media:

```shell
brew install vlc sioyek skim spotify slidepilot balenaetcher creality-slicer transmission bitwig-studio
```

Communication:

```shell
brew install zoom discord slack whatsapp messenger
```

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

### Setup Terminal.app

Get rid of the marks by selecting View > Hide Marks.

Use Option as Meta key in Settings > Profiles > Keyboard.

Make sure window closes when shell exits, Settings > Profiles > Shell.

Enlarge window size to 120 x 48, Settings > Profiles > Window > Window Size.

Set font to be a patched font, Settings > Profiles > Text > Font.

----

TODO: Port more from https://github.com/MartinHarding/macOSuckless

Control where screenshots are sent:

    mkdir -p ~/data/screenshots
    ln -s ~/data/screenshots ~/Desktop
    defaults write com.apple.screencapture ~/data/screenshots
    killall SystemUIServer
