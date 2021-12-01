function plugins#navigation#setup()
  let l:callbacks = []

  Plug 'andymass/vim-matchup'
  " %-navigate user-defined pairs

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

  if has('nvim-0.5.0')
    Plug 'ggandor/lightspeed.nvim'
    " Fancier (and more opinionated) sneak
      xmap s <Plug>Lightspeed_s
      xmap S <Plug>Lightspeed_S
      omap c <Plug>Lightspeed_s
      omap C <Plug>Lightspeed_S
      nmap , <Plug>Lightspeed_;_ft
      nmap [, <Plug>Lightspeed_;_ft
      nmap ], <Plug>Lightspeed_,_ft
    function s:SetupLightspeed()
lua <<EOF
      require'lightspeed'.setup {
        exit_after_idle_msecs = { labeled = 2500, unlabled = 1000 },
        limit_ft_matches = 16,
      }
EOF
    endfunction
    let l:callbacks += [function('s:SetupLightspeed')]
  else
    Plug 'justinmk/vim-sneak'
    " Enhanced f/t + two-character motion
      let g:sneak#label = 1                     " Easy-motion-like labels
      let g:sneak#s_next = 1                    " Empty search uses most recent
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
  endif

  if has('nvim-0.4.4')
    Plug 'nacro90/numb.nvim'
    function s:SetupNumb()
      lua require'numb'.setup()
    endfunction
    let l:callbacks += [function('s:SetupNumb')]
  endif

  return l:callbacks
endfunction
