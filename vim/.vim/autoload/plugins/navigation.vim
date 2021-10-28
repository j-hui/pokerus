function plugins#navigation#setup()

  Plug 'andymass/vim-matchup'
  " %-navigate user-defined pairs
    call g:WhichKeyIG(['%'], 'matchup-goto')

  Plug 'christoomey/vim-titlecase'
  " Title case w/ gt<motion>

  Plug 'gcmt/wildfire.vim'
  " Smart text object selection
    let g:wildfire_objects = {
          \ '*' : ["i'", "i'", 'i)', 'i]', 'i}'],
          \ 'html,xml' : ['at', 'it'],
          \}
    nmap <leader>v <Plug>(wildfire-quick-select)
    call g:WhichKeyL(['v'], 'wildfire-quick-select')

  Plug 'justinmk/vim-sneak'
  " s works like f/t but with two chars

    let g:sneak#label = 1                     " Easy-motion-like labels
    let g:sneak#s_next = 1                    " Empty search uses most recent
    let g:sneak#target_labels = ',sftunq/SFGHLTUNRMQZ?0'
    let g:sneak#prompt = 'sneak » '

    call g:WhichKeyL(['<Tab>'], 'sneak-forward')
    call g:WhichKeyL(['<BS>'], 'sneak-backward')

    " 2-character Sneak
    nmap gt <Plug>Sneak_s
    nmap gT <Plug>Sneak_S
    xmap gt <Plug>Sneak_s
    xmap gT <Plug>Sneak_S
    omap gt <Plug>Sneak_s
    omap gT <Plug>Sneak_S

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

  return []
endfunction
