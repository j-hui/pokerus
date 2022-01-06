scriptencoding utf-8

" I want a unified .vimrc for multiple environments, but also don't want to
" load more plugins than an environment can handle. So we conditionally load
" some plugins if there is anything to suggest we are running within some
" non-terminal application:
let s:env_embedded = exists('g:vscode')
" TODO: currently unused

let g:completion_tool = 'none'
if has('nvim')
  let g:colorscheme = 'edge'
  let g:lightline_colorscheme = 'edge'
else
  let g:colorscheme = 'gruvbox-material'
  let g:lightline_colorscheme = 'gruvbox_material'
endif
" let g:lightline_colorscheme = substitute(g:colorscheme, '-', '_', 'g')
let g:lightline = {
      \ 'active': {
      \   'left': [],
      \   'right': [],
      \ },
      \ 'component': {},
      \ 'component_function': {},
      \}

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

function plugins#setup()
  let l:plug_callbacks = [function('s:ColorSchemeCb')]

  call plug#begin('~/.vimplugins/plugged')

  Plug 'vim-scripts/Decho' " For debugging

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
