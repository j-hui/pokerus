set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
if !has('nvim-0.5')
  source ~/.vimrc
  finish
else
  lua require("pokerus").setup()
endif

" vim: set ts=2 sw=2 tw=80 et :
