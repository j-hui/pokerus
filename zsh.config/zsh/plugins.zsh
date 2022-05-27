#!/usr/bin/env zsh

if ! [[ -e ~/.local/share/zinit ]] ; then
    # Install zinit if it does not exist
    export NO_EDIT=y
    export NO_INPUT=y
    bash -c "$(curl --fail --show-error --silent --location https://raw.githubusercontent.com/zdharma-continuum/zinit/HEAD/scripts/install.sh)" || echo "Unable to install zinit."
    unset NO_EDIT
    unset NO_INPUT
fi

### Added by Zinit's installer
if [[ ! -f $HOME/.local/share/zinit/zinit.git/zinit.zsh ]]; then
    print -P "%F{33} %F{220}Installing %F{33}ZDHARMA-CONTINUUM%F{220} Initiative Plugin Manager (%F{33}zdharma-continuum/zinit%F{220})…%f"
    command mkdir -p "$HOME/.local/share/zinit" && command chmod g-rwX "$HOME/.local/share/zinit"
    command git clone https://github.com/zdharma-continuum/zinit "$HOME/.local/share/zinit/zinit.git" && \
        print -P "%F{33} %F{34}Installation successful.%f%b" || \
        print -P "%F{160} The clone has failed.%f%b"
fi

source "$HOME/.local/share/zinit/zinit.git/zinit.zsh"
autoload -Uz _zinit
(( ${+_comps} )) && _comps[zinit]=_zinit

# Load a few important annexes, without Turbo
# (this is currently required for annexes)
zinit light-mode for \
    zdharma-continuum/zinit-annex-as-monitor \
    zdharma-continuum/zinit-annex-bin-gem-node \
    zdharma-continuum/zinit-annex-patch-dl \
    zdharma-continuum/zinit-annex-rust
### End of Zinit's installer chunk

# zinit self-update

# A few of plugins seem to require this
autoload -U compinit

# Only check cache once per day
# https://gist.github.com/ctechols/ca1035271ad134841284
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

for p in ~/.config/zsh/plugins/*.zsh ; do
    source "$p"
done
