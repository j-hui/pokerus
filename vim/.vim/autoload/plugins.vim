scriptencoding utf-8

" I want a unified .vimrc for multiple environments, but also don't want to
" load more plugins than an environment can handle. So we conditionally load
" some plugins if there is anything to suggest we are running within some
" non-terminal application:
let s:env_embedded = exists('g:vscode')
" TODO: currently unused

let g:colorscheme = 'gruvbox-material'
let g:lightline_colorscheme = substitute(g:colorscheme, '-', '_', 'g')
let g:lightline = {
      \ 'active': {
      \   'left': [],
      \   'right': [],
      \ },
      \ 'component': {},
      \ 'component_function': {},
      \}

let g:which_key_map = { 'name': '+leader' }
let g:which_key_map_prev = { 'name': '+prev' }
let g:which_key_map_next = { 'name': '+next' }

function s:ColorSchemeCb()
  try
    exec 'colorscheme ' . g:colorscheme
  catch /^Vim\%((\a\+)\)\=:E185/
    echom g:colorscheme . ' theme not yet installed, cannot use as colorscheme.'
  endtry
endfunction

function plugins#setup()
  let l:plug_callbacks = [function('s:ColorSchemeCb')]

  call plug#begin('~/.vimplugins/plugged')

  let l:plug_callbacks += plugins#highlighting#setup()
  let l:plug_callbacks += plugins#window#setup()
  let l:plug_callbacks += plugins#ide#setup()
  let l:plug_callbacks += plugins#subsystems#setup()
  let l:plug_callbacks += plugins#files#setup()
  let l:plug_callbacks += plugins#filetypes#setup()
  let l:plug_callbacks += plugins#navigation#setup()
  let l:plug_callbacks += plugins#editing#setup()

  if filereadable(expand('~/.vim/autoload/local/plugins.vim'))
    let l:plug_callbacks += local#plugins#setup()
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
