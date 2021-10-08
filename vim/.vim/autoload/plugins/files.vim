function s:PlugDirvish()
  Plug 'justinmk/vim-dirvish'
  " Directory viewer
    Plug 'kristijanhusak/vim-dirvish-git'
    Plug 'fsharpasharp/vim-dirvinist'
    Plug 'roginfarrer/vim-dirvish-dovish', {'branch': 'main'}

    " Don't load netrw
    let g:loaded_netrw = 1
    let g:loaded_netrwPlugin = 1

    " Unmap all default mappings
    let g:dirvish_dovish_map_keys = 0

    map g- <Plug>(dirvish_up)
    function s:ConfigDirvish()
      " unmap dirvish default preview key, replace with K
      unmap <buffer> p
      nmap <silent><buffer> K   :<C-U>.call dirvish#open("p", 1)<CR>

      nmap <silent><buffer> gn  <Plug>(dirvish_git_next_file)
      nmap <silent><buffer> gp  <Plug>(dirvish_git_prev_file)

      " Your preferred mappings
      nmap <silent><buffer> i   <Plug>(dovish_create_file)
      nmap <silent><buffer> I   <Plug>(dovish_create_directory)
      nmap <silent><buffer> r   <Plug>(dovish_rename)
      nmap <silent><buffer> yy  <Plug>(dovish_yank)
      xmap <silent><buffer> yy  <Plug>(dovish_yank)
      nmap <silent><buffer> p   <Plug>(dovish_copy)
      nmap <silent><buffer> P   <Plug>(dovish_move)
      " nmap <silent><buffer> dd  <Plug>(dovish_delete)
    endfunction

    augroup dirvish_mappings
      autocmd FileType dirvish call s:ConfigDirvish()
    augroup END

  return []
endfunction

function s:PlugRooter()
  Plug 'airblade/vim-rooter'          " Automatically move pwd for projects
    let g:rooter_cd_cmd = 'lcd'
    let g:rooter_patterns = ['.git', '_darcs', '.hg', '.bzr', '.svn', 'Makefile', 'package.json', 'platformio.ini']

    " Remember where vim was initially invoked, with cmd to move back there
    let g:originalcwd = getcwd()
    command Unroot execute g:rooter_cd_cmd fnameescape(g:originalcwd)
  return []
endfunction

function plugins#files#setup()
  let l:callbacks = []
  Plug 'tpope/vim-eunuch'             " UNIX-like functionality in Vim
  Plug 'ojroques/vim-oscyank'         " Yank across the terminal
  Plug 'farmergreg/vim-lastplace'     " Open where last opened
  Plug 'duggiefresh/vim-easydir'      " Create directories when non-existent
  Plug 'strboul/urlview.vim'          " See all URLs in buffer
  Plug 'chrisbra/Recover.vim'         " See diff for recover
  Plug 'lervag/file-line'             " Open file:line
  Plug 'lambdalisue/suda.vim'         " Give vim sudo powers
    let g:suda_smart_edit = 1
  let l:callbacks += s:PlugDirvish()
  let l:callbacks += s:PlugRooter()
  return l:callbacks
endfunction
