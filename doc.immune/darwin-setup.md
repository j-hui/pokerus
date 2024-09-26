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

### Install Dependencies

The version of Bash distributed with macOS is outdated, and the `realpath` implementation
is the FreeBSD version which lacks some features that Pokerus's `git infect` depends on.
Install the necessary dependencies using:

```console
brew install coreutils bash
```

### Install Bash

```sh
brew install bash
```

### Setup SSH

Create new SSH key:

```
mkdir -p ~/.ssh &&
ssh-keygen -t ed25519 -C "j-hui@$(hostname)" -f ~/.ssh/"$(hostname)"
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

### Bash

macOS seems to use `~/.bash_profile` instead of `~/.bashrc`. Insist on the latter:

```shell
ln -s ~/.bashrc ~/.bash_profile
```

Make sure the following paths are in `PATH`:

```
export PATH
PATH="/opt/homebrew/bin:$PATH"
PATH="/opt/homebrew/opt/gnu-sed/libexec/gnubin:$PATH"
PATH="/opt/homebrew/opt/coreutils/libexec/gnubin:$PATH"
```

### Zsh

See above instructions about `PATH`.

Install some completions:

```shell
brew install zsh-completions
```

Add this to `~/.zshrc`:

```shell
if type brew &>/dev/null; then
  FPATH="$(brew --prefix)/share/zsh/site-functions:$FPATH"
  FPATH="$(brew --prefix)/share/zsh-completions:$FPATH"

  autoload -Uz compinit
  compinit
fi
```

Then load them:

```shell
rm -f ~/.zcompdump; compinit
chmod -R go-w '/opt/homebrew/share'
```

### Command-Line Utilities

Text editing and session management:

```shell
brew install vim neovim tmux tree-sitter
```

File management:

```shell
brew install ripgrep fd lsd dust diskus bat tree gnu-sed git-delta jq hexyl TankerHQ/homebrew-repo/ruplacer
```

Process management:

```shell
brew install htop bottom hyperfine
```

Version control and web:

```shell
brew install git gh subversion curl wget
```

C and embedded development:

```shell
brew install make cmake cmake-docs bear llvm ccls gcc-arm-embedded picocom
```

Other development:

```shell
brew install node go shellcheck vint lua luajit lua-language-server prettier pyright
```

Writing and media:

```shell
brew install enscript hugo imagemagick lychee texinfo texlab ltex-ls pdfgrep pdftk-java pandoc bib-tool mactex
```

### Patched Fonts

Install [Nerd Fonts](https://github.com/ryanoasis/nerd-fonts#option-3-install-script) via Homebrew:

```
brew tap homebrew/cask-fonts
brew install --cask font-hack-nerd-font font-sauce-code-pro-nerd-font font-go-mono-nerd-font font-roboto-mono-nerd-font font-comic-shanns-mono-nerd-font
```

### GUI Applications

Shells:

```shell
brew install alacritty wezterm kitty warp
```

Browsers and password managers:

```shell
brew install firefox google-chrome arc 1password
```

Customization:

```shell
brew install rectangle keycastr alt-tab
```

Media:

```shell
brew install vlc sioyek skim spotify slidepilot balenaetcher creality-slicer transmission bitwig-studio steam
```

Communication:

```shell
brew install zoom discord slack whatsapp messenger
```

Virtualization:

```shell
brew install docker
```

### Fast `KeyRepeat`

```shell
defaults write -g KeyRepeat -int 1
defaults write -g InitialKeyRepeat -int 20
defaults write -g ApplePressAndHoldEnabled -bool false
```

Note that the normal minimum `KeyRepeat` is 2 (30 ms)
and the normal minimum `InitialKeyRepeat` is 15 (225 ms).

### Show file extensions

```shell
defaults write -g AppleShowAllExtensions -bool true
```

### Finder in home directory

```shell
defaults read com.apple.finder NewWindowTargetPath -string file:///Users/`whoami`
```

> **NOTE** this doesn't seem to work from CLI, but is easily configured in
> Finder's preferences.

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
