function pokerus#plugins#speeddating#setup()
  let g:speeddating_no_mappings = 1
  " force a non-recursive map to the fallback functions
  nnoremap <Plug>SpeedDatingFallbackUp    <C-A>
  nnoremap <Plug>SpeedDatingFallbackDown  <C-X>
  xnoremap <Plug>SpeedDatingFallbackUp    <C-A>
  xnoremap <Plug>SpeedDatingFallbackDown  <C-X>
  nmap + <Plug>SpeedDatingUp
  nmap - <Plug>SpeedDatingDown
  xmap + <Plug>SpeedDatingUp
  xmap - <Plug>SpeedDatingDown
endfunction
