" ============================================================================
" Pokerus .vimrc (by j-hui)
" ============================================================================
" vint: -ProhibitSetNoCompatible

if 0
 " Change to 1 if doing a minimal .vimrc test
 set nocompatible
 filetype plugin indent on
 syntax enable
 call plug#begin('~/.vimplugins/plugged')
  " Add specific plugins here
 call plug#end()
 finish
endif

" Note: these are already turned on by vim-plug
set nocompatible
filetype plugin on
syntax on

" Define this before anything else, since it is effectful
let g:mapleader = "\<Space>"

" These are defined in .vim/autoload/
call pokerus#plugins#setup()
call pokerus#settings#setup()
call pokerus#keybinds#setup()
call pokerus#commands#setup()

" Avoid E173 (vim refuses to close with unvisited buffers)
if argc() > 1
  silent blast " load last buffer
  silent bfirst " switch back to the first
endif

" vim: set ts=2 sw=2 tw=120 et foldmethod=marker foldlevel=0:
