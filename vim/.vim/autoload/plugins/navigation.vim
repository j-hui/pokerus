function plugins#navigation#setup()
  let l:callbacks = []

  Plug 'kana/vim-textobj-user'
  " User-defined motions beginning with {a,i}
  " NOTE: double check against text objects given by treesitter

  Plug 'thinca/vim-textobj-between'
  " {a,i} f<char> motions for region between <char>

  Plug 'Julian/vim-textobj-variable-segment'
  " {a,i} v motions for portions of variables

  Plug 'kana/vim-textobj-indent'
  " {a,i} i motions for indented blocks of text

  Plug 'rhysd/vim-textobj-continuous-line'
  " motions for navigating line continuations
    let g:textobj_continuous_line_no_default_key_mappings = 1
    let g:textobj_continuous_line_no_default_mappings = 1
    omap <buffer>a\ <Plug>(textobj-continuous-vim-a)
    vmap <buffer>a\ <Plug>(textobj-continuous-vim-a)
    omap <buffer>i\ <Plug>(textobj-continuous-vim-i)
    vmap <buffer>i\ <Plug>(textobj-continuous-vim-i)

  Plug 'saihoooooooo/vim-textobj-space'
  " motions for continuous stretches of space
    let g:textobj_space_no_default_key_mappings = 1
    omap <buffer>a<space> <Plug>(textobj-space-a)
    vmap <buffer>a<space> <Plug>(textobj-space-a)
    omap <buffer>i<space> <Plug>(textobj-space-i)
    vmap <buffer>i<space> <Plug>(textobj-space-i)

  Plug 'kana/vim-textobj-syntax'
  " {a,i} y motions for syntax items

  Plug 'kana/vim-textobj-datetime'
  " {a,i} da, dd, df, dt, dz motions for date/time

  Plug 'rbonvall/vim-textobj-latex'
  " {a,i} $, q/Q, and e motions for latex math, quotes, and environments

  Plug 'preservim/vim-textobj-sentence'
  " {a,s} motions for modern prose sentences
    augroup textobj_sentence
      autocmd!
      autocmd FileType markdown call textobj#sentence#init()
      autocmd FileType textile call textobj#sentence#init()
      autocmd FileType tex call textobj#sentence#init()
    augroup END

  Plug 'paulhybryant/vim-textobj-path'
  " {a,i} {p,P} for path names

  Plug 'adriaanzon/vim-textobj-matchit'
  " {a,i} m for matchit pairs
    let g:textobj_matchit_no_default_key_mappings = 1
    xmap a%  <Plug>(textobj-matchit-a)
    omap a%  <Plug>(textobj-matchit-a)
    xmap i%  <Plug>(textobj-matchit-i)
    omap i%  <Plug>(textobj-matchit-i)

  Plug 'andymass/vim-matchup'
  " %-navigate user-defined pairs

  Plug 'christoomey/vim-titlecase'
  " Title case w/ gz<motion>

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
