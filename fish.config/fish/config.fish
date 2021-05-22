### Colors {{{
# https://fishshell.com/docs/current/index.html#variables-for-changing-highlighting-colors

# https://draculatheme.com/fish
set -l foreground f8f8f2
set -l selection  44475a
set -l comment    6272a4
set -l red    ff5555
set -l orange ffb86c
set -l yellow f4f99d
set -l green  50fa7b
set -l cyan   8be9fd
set -l pink   ff79c6
set -l purple bd93f9

# Syntax highlighting colors
set -g fish_color_normal $foreground
set -g fish_color_command $cyan
set -g fish_color_quote $yellow
set -g fish_color_redirection $foreground
set -g fish_color_end $orange
set -g fish_color_error $red
set -g fish_color_param $purple
set -g fish_color_comment $comment
set -g fish_color_match --background=$selection
set -g fish_color_selection --background=$selection
set -g fish_color_search_match --background=$selection
set -g fish_color_operator $green
set -g fish_color_escape $pink
set -g fish_color_autosuggestion $comment
set -g fish_color_cancel $comment

# Completion pager colors
set -g fish_pager_color_progress $comment
set -g fish_pager_color_prefix $cyan
set -g fish_pager_color_completion $foreground
set -g fish_pager_color_description $comment

# Prompt colors
set -g fish_color_ps1 $comment
set -g fish_color_date $comment
set -g fish_color_user $orange
set -g fish_color_host $cyan
set -g fish_color_cwd $yellow
set -g fish_color_cwd_root $orange
set -g fish_color_status $red
### Colors }}}

### Prompt {{{
# Symbols
set -g fish_prompt_suffix '⮞'
set -g fish_prompt_return '⮜'
set -g fish_prompt_suffix_root '#'

# No prompt greeting or vi mode prompt
set fish_greeting
set fish_mode_prompt

### Prompt }}}

### Utilities {{{

## Export variables {{{
set -gx NAME "j-hui"
set -gx EMAIL "j-hui@cs.columbia.edu"
abbr --add o open

# Note: Unlike other shells, $PATH is a list, not a colon-delimited string.
set PATH ~/bin $PATH
set PATH ~/.cargo/bin $PATH
set PATH ~/go/bin $PATH
set PATH ~/.local/bin $PATH
set PATH ~/.cabal/bin $PATH
set PATH ~/.ghcup/bin $PATH

## Export variables }}}

## FZF, bat, and fd {{{

set -gx FZF_DEFAULT_OPTS '--bind=ctrl-k:kill-line,alt-a:select-all,alt-e:deselect-all,ctrl-space:toggle --marker=*'

if command -v bat >/dev/null
    set -gx FZF_CTRL_T_OPTS "--preview 'bat --style=numbers --color=always {} | head -500'"
    set -gx MANPAGER "sh -c 'col -bx | bat -l man -p --paging always'"
    # NOTE: MANPAGER will be later overridden if we have nvim installed
    abbr --add b bat
else
    set -gx FZF_CTRL_T_OPTS "--preview 'cat {}'"
end

if command -v fd >/dev/null
    set -gx FZF_DEFAULT_COMMAND 'fd --type f --hidden --follow --exclude .git'
    set -gx FZF_CTRL_T_COMMAND "$FZF_DEFAULT_COMMAND"
    set -gx FZF_ALT_C_COMMAND "$FZF_DEFAULT_COMMAND"
end
## FZF }}}

## Text editor {{{
if command -v nvim >/dev/null
    set -gx EDITOR nvim
    set -gx MANPAGER 'nvim +Man!'    # use nvim as man pager
    set -gx MANWIDTH 999             # let nvim handle wraparound

    abbr --add e nvim
    abbr --add vim nvim

else if command -v vim >/dev/null
    set -gx EDITOR vim
    abbr --add e vim

else
    echo "Could not locate preferred editors..."
end
mkdir -p ~/.tmp/backup ~/.tmp/swp ~/.tmp/undo
## Text editor }}}

## Kitty {{{
if command -v kitty >/dev/null
    abbr --add k kitty +kitten

    alias ssh='kitty +kitten ssh'
    alias icat='kitty +kitten icat --align left'
end
## Kitty }}}

## Exa {{{
if command -v exa >/dev/null
    abbr --add l 'exa -F'
    abbr --add ll 'exa -alF'
    abbr --add la 'exa -aF'
end
## Exa }}}

## Stack {{{
abbr --add stacki stack --no-nix-pure ghci
# Stack }}}

## nnn {{{
set -gx NNN_FIFO '/tmp/nnn.fifo'
set -gx NNN_PLUG 'o:fzopen;f:fzfcd;m:mimelist;p:preview-tabbed;d:dragdrop;u:dups'
# nnn }}}

## Less {{{
# X = leave content on-screen
# F = quit automatically if less than one screenfull
# R = raw terminal characters (fixes git diff)
#     see http://jugglingbits.wordpress.com/2010/03/24/a-better-less-playing-nice-with-git/
set -gx LESS "-F -X -R"
set -gx LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
set -gx LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
set -gx LESS_TERMCAP_me \e'[0m'           # end mode
set -gx LESS_TERMCAP_se \e'[0m'           # end standout-mode
set -gx LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
set -gx LESS_TERMCAP_ue \e'[0m'           # end underline
set -gx LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline
## Less }}}

## Misc Utilities {{{
set -gx RIPGREP_CONFIG_PATH ~/.config/ripgrep/ripgreprc
source ~/.opam/opam-init/init.fish > /dev/null 2> /dev/null; or true
if command -v prettyping >/dev/null; abbr --add pping 'prettyping --nolegend'; end

## Misc Utilities }}}

### Utilities }}}

### Misc {{{

umask 077

# Fish should not add things to clipboard when killing
# See https://github.com/fish-shell/fish-shell/issues/772
set FISH_CLIPBOARD_CMD "cat"

### Misc }}}

# vim: set ft=fish foldmethod=marker:
