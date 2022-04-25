set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
if !has('nvim-0.5')
  source ~/.vimrc
  finish
else
  let g:mapleader = "\<Space>"
  if empty(glob(stdpath('data') . '/site/pack/packer/start/impatient.nvim')) == 0
     lua require('impatient').enable_profile()
  endif
lua <<EOF
  require("pokerus").setup()
EOF
  call pokerus#settings#setup()
  call pokerus#keybinds#setup()
  call pokerus#commands#setup()
endif

" Avoid E173 (n more file(s) to edit)
if argc() > 1
 silent blast   " load last buffer
 silent bfirst  " switch back to the first
endif
