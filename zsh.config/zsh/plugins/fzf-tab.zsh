#!/usr/bin/env zsh

# NOTE: :completion:<function>:<completer>:<command>:<argument>:<tag>

zmodload zsh/complist

# Ensure fzf is installed
if ! [ -f ~/.fzf.zsh ] ; then
  if command -v git > /dev/null ; then
    git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  else
    mkdir -p ~/.fzf/
    curl --fail --show-error --silent --location https://raw.githubusercontent.com/junegunn/fzf/master/install > ~/.fzf/install
  fi
  ~/.fzf/install --all --no-update-rc
fi

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

zinit light Aloxaf/fzf-tab
zinit light zsh-users/zsh-autosuggestions
zinit light zsh-users/zsh-completions

# Bindings
zstyle ':fzf-tab:*' fzf-bindings 'ctrl-u:cancel' 'ctrl-a:cancel' 'ctrl-e:accept' 'ctrl-l:accept'

# Automatically accept with enter for cd
zstyle ':fzf-tab:*:cd:*' accept-line enter

# Use typed query instead of selected entry
zstyle ':fzf-tab:*' print-query ctrl-j

# Colors
zstyle ':fzf-tab:*' default-color $'\033[34m'

# Previewing

## Directories
if command -v exa &> /dev/null ; then
    zstyle ':fzf-tab:complete:cd:*' fzf-preview 'exa -1 --color=always $realpath'
else
    zstyle ':fzf-tab:complete:cd:*' fzf-preview 'ls -1 --color=always $realpath'
fi

## ps and kill
zstyle ':completion:*:*:*:*:processes' command "ps -u $USER -o pid,user,comm -w -w"
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-preview \
  '[[ $group == "[process ID]" ]] && ps --pid=$word -o cmd --no-headers -w -w'
zstyle ':fzf-tab:complete:(kill|ps):argument-rest' fzf-flags --preview-window=down:3:wrap

## systemd
zstyle ':fzf-tab:complete:systemctl-*:*' fzf-preview 'SYSTEMD_COLORS=1 systemctl status $word'

## Variables
zstyle ':fzf-tab:complete:(-command-|-parameter-|-brace-parameter-|export|unset|expand):*' \
    fzf-preview 'echo ${(P)word}'

## Git
zstyle ':fzf-tab:complete:git-(add|diff|restore):*' fzf-preview \
    'git diff $word | delta'
zstyle ':fzf-tab:complete:git-log:*' fzf-preview \
    'git log --color=always $word'
zstyle ':fzf-tab:complete:git-help:*' fzf-preview \
    'git help $word | bat -plman --color=always'
zstyle ':fzf-tab:complete:git-show:*' fzf-preview \
    'case "$" in
    "commit tag") git show --color=always $word ;;
    *) git show --color=always $word | delta ;;
    esac'
zstyle ':fzf-tab:complete:git-checkout:*' fzf-preview \
    'case "$" in
    "modified file") git diff $word | delta ;;
    "recent commit object name") git show --color=always $word | delta ;;
    *) git log --color=always $word ;;
    esac'

# Below: depenency-free completion. But it's just nicer to use fzf
#
# # Cache completions
# zstyle ':completion:*' use-cache on
# zstyle ':completion:*' cache-path ~/.cache/zsh/.zcompcache
#
# zstyle ':completion:*' completer _extensions _complete _approximate
#
# # zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
#
# # disable sort when completing `git checkout`
# zstyle ':completion:*:git-checkout:*' sort false
#
# # Use menu selection UI
# zstyle ':completion:*' menu select=0 search
# # setopt MENU_COMPLETE
# # setopt MENU_COMPLETE
# # setopt GLOB_COMPLETE
#
# zstyle ':completion:*' group-name ''
#
# # Colors, taken from: https://stackoverflow.com/questions/23152157/how-does-the-zsh-list-colors-syntax-work
# zstyle ':completion:*:parameters'  list-colors '=*=32'
# zstyle ':completion:*:commands' list-colors '=*=1;31'
# zstyle ':completion:*:builtins' list-colors '=*=1;38;l5;142'
# zstyle ':completion:*:aliases' list-colors '=*=2;38;5;128'
# zstyle ':completion:*:*:kill:*' list-colors '=(#b) #([0-9]#)*( *[a-z])*=34=31=33'
# zstyle ':completion:*:options' list-colors '=^(-- *)=34'
# zstyle ":completion:*:default" list-colors ${(s.:.)LS_COLORS} "ma=48;5;153;1"
#
# # Some bindings to conform to my impulses
# bindkey -M menuselect '^q' send-break
# bindkey -M menuselect '^k' send-break
# bindkey -M menuselect '^e' accept-line
# bindkey -M menuselect '^l' accept-and-hold
# bindkey -M menuselect '^j' accept-and-hold
