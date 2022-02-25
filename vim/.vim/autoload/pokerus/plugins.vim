scriptencoding utf-8

let g:completion_tool = 'none'
if has('nvim')
  let g:colorscheme = 'edge'
else
  let g:colorscheme = 'gruvbox-material'
endif

let g:which_key_map = {}
let g:which_key_map_v = {}
let g:which_key_map_i = {}

" Construct which-key mappings
function g:WhichKey(mode, keys, mapping)
  if len(a:keys) < 2
    " No business handling mappings with fewer than 2 keys
    return
  endif

  if a:mode ==# 'n'
    let l:map = g:which_key_map
  elseif a:mode ==# 'v'
    let l:map = g:which_key_map_v
  elseif a:mode ==# 'i'
    let l:map = g:which_key_map_i
  endif

  for l:key in a:keys[:len(a:keys)-2]
    if l:key ==# '<leader>'
      let l:key = g:mapleader
    elseif l:key ==# 'name'
      " the key 'name' should not appear anywhere before the end
      echoerr 'Cannot use key "name" in middle of keys: ' . string(keys)
      return
    endif
    if ! has_key(l:map, l:key)
      let l:map[l:key] = {}
    endif
    let l:map = l:map[l:key]
  endfor
  let l:map[a:keys[len(a:keys)-1]] = a:mapping
endfunction

function g:WhichKeyL(keys, mapping)
  call g:WhichKey('n', ['<leader>'] + a:keys, a:mapping)
endfunction

function g:WhichKeyIG(keys, mapping)
  call g:WhichKey('i', ['<C-g>'] + a:keys, a:mapping)
endfunction

function s:ColorSchemeCb()
  try
    exec 'colorscheme ' . g:colorscheme
  catch /^Vim\%((\a\+)\)\=:E185/
    echom g:colorscheme . ' theme not yet installed, cannot use as colorscheme.'
  endtry
endfunction

function s:SetupVimPlugins()
  call plug#begin('~/.vimplugins/plugged')

  " Vim-specific {{{
    Plug 'morhetz/gruvbox'
    " A classic theme

    Plug 'ourigen/skyline.vim'
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

    Plug 'svermeulen/vim-cutlass'
    " x and D only delete, no yank/cut but retain cut behavior for d

    Plug 'machakann/vim-sandwich'
    " Fancier vim-surround + d

    Plug 'ojroques/vim-oscyank'
    " Yank across the terminal

    Plug 'junegunn/fzf.vim'
    Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
    " Fuzzy finder

    Plug 'tpope/vim-fugitive'
    " tpope's illegal git plugin

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

  call pokerus#plugins#cutlass#setup()
  call pokerus#plugins#fugitive#setup()
  call pokerus#plugins#fzf#setup()
  call pokerus#plugins#oscyank#setup()
  call pokerus#plugins#sandwich#setup()
  call pokerus#plugins#sneak#setup()
  call pokerus#plugins#speeddating#setup()
  call pokerus#plugins#subversive#setup()
  call pokerus#plugins#suda#setup()

  call pokerus#plugins#cpp_enhanced#setup()
  call pokerus#plugins#haskell#setup()
  call pokerus#plugins#go#setup()
  call pokerus#plugins#zig#setup()
  call pokerus#plugins#markdown#setup()
endfunction

function pokerus#plugins#setup()
  if !has('nvim-0.5.0')
    call s:SetupVimPlugins()
    return
  endif
  let l:plug_callbacks = [function('s:ColorSchemeCb')]

  call plug#begin('~/.vimplugins/plugged')

  " Plug 'vim-scripts/Decho'
  " Plugin debugging
  " Commented out because this defines the mapping <leader>rwp for some reason
  Plug 'dstein64/vim-startuptime'
  " Startup time profiling
  Plug 'norcalli/profiler.nvim'
  " Lua code profiling

  let l:plug_callbacks += plugins#subsystems#setup()
  let l:plug_callbacks += plugins#highlighting#setup()
  let l:plug_callbacks += plugins#window#setup()
  let l:plug_callbacks += plugins#ide#setup()
  let l:plug_callbacks += plugins#files#setup()
  let l:plug_callbacks += plugins#filetypes#setup()
  let l:plug_callbacks += plugins#navigation#setup()
  let l:plug_callbacks += plugins#editing#setup()

  if filereadable(expand('~/.vim/autoload/local.vim'))
    let l:plug_callbacks += local#plugins_setup()
  endif

  call plug#end()

  " try
  for Cb in l:plug_callbacks
    call Cb()
  endfor
  " catch
  "     echom 'Encountered errors when executing plug callbacks, run :PlugInstall'
  " endtry
endfunction

" vim: set ts=2 sw=2 tw=80 et foldmethod=marker foldlevel=0 :
