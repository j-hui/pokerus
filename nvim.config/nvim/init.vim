set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
if !has('nvim-0.5')
  source ~/.vimrc
  finish
else
  let g:mapleader = "\<Space>"
  " luafile /home/j-hui/extern/profiler.nvim/lua/profiler.lua
lua <<EOF
  require "pokerus.config"
  require "pokerus.packer"
EOF
  call pokerus#settings#setup()
  call pokerus#keybinds#setup()
  call pokerus#commands#setup()
endif
