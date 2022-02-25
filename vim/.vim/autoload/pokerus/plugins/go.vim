function pokerus#plugins#go#setup()
  " Disable features to let LSP configuration do the work
  let g:go_echo_go_info = 0
  let g:go_code_completion_enabled = 0
  let g:go_fmt_autosave = 0
  let g:go_gopls_enabled = 0
  let g:go_def_mapping_enabled = 0
endfunction
