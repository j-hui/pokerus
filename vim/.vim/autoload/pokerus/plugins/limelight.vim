function pokerus#plugins#limelight#setup()
  nmap <leader>L :Limelight!!<CR>
  xmap <leader>L <Plug>(Limelight)

  let g:limelight_conceal_ctermfg = 'gray'
  let g:limelight_conceal_ctermfg = 240
endfunction
