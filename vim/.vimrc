" ============================================================================
" Pokerus .vimrc (by J-Hui)
" ============================================================================

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

" These are defined in .vim/autoload/
call plugins#setup()
call settings#setup()
call keybinds#setup()
call commands#setup()
call spelling#setup()

" Avoid E173 (vim refuses to close with unvisited buffers)
if argc() > 1
  silent blast " load last buffer
  silent bfirst " switch back to the first
endif

" vim: set ts=2 sw=2 tw=120 et foldmethod=marker foldlevel=0:
