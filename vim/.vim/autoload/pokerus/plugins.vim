scriptencoding utf-8

function pokerus#plugins#setup()
  call plug#begin('~/.vimplugins/plugged')

  " Vim-specific {{{
    Plug 'morhetz/gruvbox'
    " A classic theme

    Plug 'j-hui/skyline.vim'
    " Lightweight status line that takes the guesswork out of configuration

    Plug 'ap/vim-buftabline'
    " Tab bar at top

    Plug 'tpope/vim-commentary'
    " use gcc to comment things out

    Plug 'suy/vim-context-commentstring'
    " comments for embedded syntax

    Plug 'junegunn/vim-peekaboo'
    " See yank registers

    Plug 'justinmk/vim-sneak'
    " Enhanced f/t + two-character motion

    Plug 'tpope/vim-vinegar'
    " Make netrw usable

    Plug 'farmergreg/vim-lastplace'
    " Open where last opened

    Plug 'tpope/vim-speeddating'
    " increment/decrement dates

  " Vim-specific }}}

  " Vim + Neovim (no setup) {{{

    Plug 'tpope/vim-unimpaired'
    " ]* and [* mappings

    Plug 'tpope/vim-endwise'
    " Automatically add closing token where appropriate

    Plug 'tpope/vim-abolish'
    " Smarter subtitutions

    Plug 'tpope/vim-eunuch'
    " UNIX-like functionality in Vim

    Plug 'tommcdo/vim-exchange'
    " Exchange text with repeated cx{motion}

    Plug 'duggiefresh/vim-easydir'
    " Create directories when non-existent

    Plug 'lervag/file-line'
    " Open file:line

    Plug 'andymass/vim-matchup'
    " %-navigate user-defined pairs

    Plug 'christoomey/vim-titlecase'
    " Title case w/ gz<motion>

    Plug 'cosminadrianpopescu/vim-tail'
    " Make vim behave like tail -f

    Plug 'guns/xterm-color-table.vim'
    " Preview all 256 xterm colors

    Plug 'moll/vim-bbye'
    " Delete buffers without messing up buffer layout

    Plug 'AndrewRadev/bufferize.vim'
    " Command contents in buffer

    Plug 'AndrewRadev/linediff.vim'
    " Vimdiff line ranges

  " Vim + Neovim (no setup) }}}

  " Vim + Neovim (w/ setup) {{{
    Plug 'lambdalisue/suda.vim'
    " Give vim sudo powers

    Plug 'svermeulen/vim-subversive'
    " Substitute from yank

    Plug 'machakann/vim-sandwich'
    " Fancier vim-surround + d

    Plug 'ojroques/vim-oscyank'
    " Yank across the terminal

    Plug 'junegunn/fzf.vim'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    " Fuzzy finder

    Plug 'tpope/vim-fugitive'
    " tpope's illegal git plugin

    Plug 'junegunn/limelight.vim'
    " Dim out distractions

  " Vim + Neovim (w/ setup) }}}

  " Filetype {{{
    Plug 'leanprover/lean.vim'
    Plug 'idris-hackers/idris-vim'
    Plug 'LnL7/vim-nix'
    Plug 'ziglang/zig.vim'
    Plug 'baskerville/vim-sxhkdrc'
    Plug 'blyoa/vim-promela-syntax'
    Plug 'chrisbra/csv.vim'
    Plug 'rust-lang/rust.vim'
    Plug 'leafgarland/typescript-vim'
    Plug 'keith/swift.vim'
    Plug 'dag/vim-fish'
    Plug 'cespare/vim-toml'
    Plug 'neomutt/neomutt.vim'
    Plug 'liuchengxu/graphviz.vim'
    Plug 'editorconfig/editorconfig-vim'
    Plug 'mboughaba/i3config.vim'
    Plug 'fatih/vim-go'
    Plug 'octol/vim-cpp-enhanced-highlight'
    Plug 'neovimhaskell/haskell-vim'
    Plug 'andy-morris/happy.vim'
    Plug 'andy-morris/alex.vim'
    Plug 'preservim/vim-markdown', { 'as': 'preservim-vim-markdown' }
    Plug 'euclidianAce/BetterLua.vim'
    Plug 'whonore/coqtail'
  " Filetype }}}
  call plug#end()

  colo gruvbox

  call pokerus#plugins#buftabline#setup()
  call pokerus#plugins#skyline#setup()

  call pokerus#plugins#fugitive#setup()
  call pokerus#plugins#fzf#setup()
  call pokerus#plugins#oscyank#setup()
  call pokerus#plugins#sandwich#setup()
  call pokerus#plugins#sneak#setup()
  call pokerus#plugins#speeddating#setup()
  call pokerus#plugins#subversive#setup()
  call pokerus#plugins#suda#setup()
  call pokerus#plugins#limelight#setup()

  call pokerus#plugins#cpp_enhanced#setup()
  call pokerus#plugins#haskell#setup()
  call pokerus#plugins#go#setup()
  call pokerus#plugins#zig#setup()
  call pokerus#plugins#markdown#setup()

endfunction

" vim: set ts=2 sw=2 tw=80 et foldmethod=marker foldlevel=0 :
