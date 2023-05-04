function! s:OSCYankHandler(event) abort
  if v:event.operator is# 'y' && a:event.regname is# ''
    execute 'OSCYankRegister "'
  endif
endfunction

function! pokerus#plugins#oscyank#setup() abort
  let g:oscyank_trim = 0
  augroup osc-yank
    autocmd!
    autocmd TextYankPost * call s:OSCYankHandler(v:event)
  augroup END
endfunction
