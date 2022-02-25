scriptencoding utf-8
function pokerus#plugins#sneak#setup()
  let g:sneak#label = 1   " Easy-motion-like labels
  let g:sneak#s_next = 1  " Empty search uses most recent
  let g:sneak#target_labels = ',sftunq/SFGHLTUNRMQZ'
  let g:sneak#prompt = 'sneak » '

  " 2-character Sneak
  nmap s <Plug>Sneak_s
  nmap S <Plug>Sneak_S
  xmap s <Plug>Sneak_s
  xmap S <Plug>Sneak_S
  omap c <Plug>Sneak_s
  omap C <Plug>Sneak_S

  " Already mapped ; to :
  map ,   <Plug>Sneak_;
  map ],  <Plug>Sneak_;
  map [,  <Plug>Sneak_,

  " 1-character enhanced 'f'
  nmap f <Plug>Sneak_f
  nmap F <Plug>Sneak_F
  xmap f <Plug>Sneak_f
  xmap F <Plug>Sneak_F
  omap f <Plug>Sneak_f
  omap F <Plug>Sneak_F

  " 1-character enhanced 't'
  nmap t <Plug>Sneak_t
  nmap T <Plug>Sneak_T
  xmap t <Plug>Sneak_t
  xmap T <Plug>Sneak_T
  omap t <Plug>Sneak_t
  omap T <Plug>Sneak_T
  omap T <Plug>Sneak_T
endfunction
