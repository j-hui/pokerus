
function s:PlugDirvish()
  Plug 'justinmk/vim-dirvish'         " Lightweight directory viewer
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

function s:PlugVinegar()
  Plug 'tpope/vim-vinegar'            " Make netrw usable
  return []
endfunction

function s:PlugFern()
  Plug 'lambdalisue/fern.vim'
    " Modern filetree plugin
    let g:fern#disable_default_mappings = 1
    let g:fern#hide_cursor = 1
    let g:fern#mark_symbol = '>>'

  Plug 'antoinemadec/FixCursorHold.nvim'
    " Fix CursorHold performance bug

  Plug 'hrsh7th/fern-mapping-collapse-or-leave.vim'
    " collapses or leaves
    let g:fern#mapping#collapse_or_leave#disable_default_mappings = 1

  Plug 'lambdalisue/fern-hijack.vim'
    " Hijack netrw

  Plug 'lambdalisue/nerdfont.vim'
  Plug 'lambdalisue/fern-renderer-nerdfont.vim'
    " Nice icons
    let g:fern#renderer = 'nerdfont'

  Plug 'lambdalisue/fern-git-status.vim'
    " Show git status
    let g:fern_git_status#indexed_character = '-'
    let g:fern_git_status#stained_character = '!'

  Plug 'lambdalisue/fern-mapping-git.vim'
    " >> to stage, << to unstage

  Plug 'lambdalisue/fern-ssh'
    " View files over ssh

  Plug 'yuki-yano/fern-preview.vim'
    " Preview files

  Plug 'LumaKernel/fern-mapping-fzf.vim'
    " Pick using fzf
    let g:fern#mapping#fzf#disable_default_mappings = 1

  Plug 'lambdalisue/fern-mapping-project-top.vim'
    " Open the git project directory

  nnoremap g=     :Fern -drawer -reveal=% .<CR>
    " Open current working directory in drawer
  nnoremap g+     :Fern -drawer -reveal=% %:h<CR>
    " Open current file's parent directory in drawer
  nnoremap g-     :Fern -reveal=% .<CR>
    " Open current working directory
  nnoremap g<BS>  :Fern -reveal=% %:h<CR>
    " Open current file's parent directory

  call g:WhichKey('n', ['g', '='],    'fern-drawer-cwd')
  call g:WhichKey('n', ['g', '+'],    'fern-drawer-parent')
  call g:WhichKey('n', ['g', '-'],    'fern-open-cwd')
  call g:WhichKey('n', ['g', '<BS>'], 'fern-open-parent')

  function! s:SetupFern() abort
    nmap <buffer>
          \ <Plug>(fern-action-open)
          \ <Plug>(fern-action-open:select)
    nmap <buffer>
          \ <Plug>(fern-action-terminal)
          \ <Plug>(fern-action-terminal:select)
    nmap <buffer>
          \ <Plug>(fern-action-diff)
          \ <Plug>(fern-action-diff:select:vert)
    nmap <silent><buffer> <expr>
          \ <Plug>(fern-quit-or-close-preview)
          \ fern_preview#smart_preview("\<Plug>(fern-action-preview:close)", ":q\<CR>")

    nmap <silent><buffer>         g=      :q<CR>
    nmap <silent><buffer>         q       <Plug>(fern-quit-or-close-preview)
    nmap <silent><buffer><nowait> <C-c>   <Plug>(fern-action-cancel)
    nmap <silent><buffer><nowait> <C-l>   <Plug>(fern-action-redraw)

    nmap <silent><buffer><nowait> <C-m>   <Plug>(fern-action-open-or-enter)
    nmap <silent><buffer><nowait> J       <Plug>(fern-action-open-or-expand)
    nmap <silent><buffer><nowait> F       <Plug>(fern-action-open)
    nmap <silent><buffer><nowait> ff      <Plug>(fern-action-open:edit)
    nmap <silent><buffer><nowait> fs      <Plug>(fern-action-open:split)
    nmap <silent><buffer><nowait> fv      <Plug>(fern-action-open:vsplit)
    nmap <silent><buffer><nowait> fk      <Plug>(fern-action-open:above)
    nmap <silent><buffer><nowait> fh      <Plug>(fern-action-open:left)
    nmap <silent><buffer><nowait> fj      <Plug>(fern-action-open:below)
    nmap <silent><buffer><nowait> fl      <Plug>(fern-action-open:right)
    nmap <silent><buffer><nowait> f!      <Plug>(fern-action-open:system)

    nmap <silent><buffer>         T       <Plug>(fern-action-terminal)
    nmap <silent><buffer><nowait> tt      <Plug>(fern-action-terminal:edit)
    nmap <silent><buffer><nowait> ts      <Plug>(fern-action-terminal:split)
    nmap <silent><buffer><nowait> tv      <Plug>(fern-action-terminal:vsplit)
    nmap <silent><buffer><nowait> tk      <Plug>(fern-action-terminal:above)
    nmap <silent><buffer><nowait> th      <Plug>(fern-action-terminal:left)
    nmap <silent><buffer><nowait> tj      <Plug>(fern-action-terminal:below)
    nmap <silent><buffer><nowait> tl      <Plug>(fern-action-terminal:right)

    nmap <silent><buffer>         D       <Plug>(fern-action-diff)
    nmap <silent><buffer><nowait> dd      <Plug>(fern-action-diff:edit)
    nmap <silent><buffer><nowait> ds      <Plug>(fern-action-diff:split)
    nmap <silent><buffer><nowait> dv      <Plug>(fern-action-diff:vsplit)
    nmap <silent><buffer><nowait> dk      <Plug>(fern-action-diff:above)
    nmap <silent><buffer><nowait> dh      <Plug>(fern-action-diff:left)
    nmap <silent><buffer><nowait> dj      <Plug>(fern-action-diff:below)
    nmap <silent><buffer><nowait> dl      <Plug>(fern-action-diff:right)

    nmap <silent><buffer>         =       <Plug>(fern-action-cd:cursor)
    nmap <silent><buffer>         -       <Plug>(fern-action-cd:root)

    nmap <silent><buffer>         <BS>    <Plug>(fern-action-leave)
    nmap <silent><buffer>         g<BS>   <Plug>(fern-action-leave)
    nmap <silent><buffer>         u       <Plug>(fern-action-leave)
    nmap <silent><buffer>         b       <Plug>(fern-action-focus:parent)
    nmap <silent><buffer>         K       <Plug>(fern-action-collapse-or-leave)
    nmap <silent><buffer>         g-      <Plug>(fern-action-collapse-or-leave)
    nmap <silent><buffer>         g_      <Plug>(fern-action-project-top:reveal)

    nmap <silent><buffer>         s       <Plug>(fern-action-fzf-both)
    nmap <silent><buffer>         /       <Plug>(fern-action-grep)

    " nmap <silent> <buffer> F <Plug>(fern-action-fzf-files)
    " nmap <silent> <buffer> d <Plug>(fern-action-fzf-dirs)

    nmap <silent><buffer><nowait> !      <Plug>(fern-action-hidden)
    nmap <silent><buffer><nowait> i      <Plug>(fern-action-include)
    nmap <silent><buffer><nowait> o      <Plug>(fern-action-exclude)

    nmap <silent><buffer><nowait> m       <Plug>(fern-action-mark)
    vmap <silent><buffer><nowait> m       <Plug>(fern-action-mark)
    nmap <silent><buffer><nowait> <Tab>   <Plug>(fern-action-mark)
    vmap <silent><buffer><nowait> <Tab>   <Plug>(fern-action-mark)

    nmap <silent><buffer><nowait> o       <Plug>(fern-action-new-path)
    nmap <silent><buffer><nowait> O       <Plug>(fern-action-new-path)
    nmap <silent><buffer><nowait> y.      <Plug>(fern-action-yank)
    nmap <silent><buffer><nowait> yc      <Plug>(fern-action-copy)
    nmap <silent><buffer><nowait> ym      <Plug>(fern-action-move)
    nmap <silent><buffer><nowait> x       <Plug>(fern-action-trash)

    nmap <silent><buffer>         p       <Plug>(fern-action-preview:toggle)
    nmap <silent><buffer>         P       <Plug>(fern-action-preview:auto:toggle)
    nmap <silent><buffer>         <C-d>   <Plug>(fern-action-preview:scroll:down:half)
    nmap <silent><buffer>         <C-u>   <Plug>(fern-action-preview:scroll:up:half)

    nmap <buffer><nowait>         <<      <Plug>(fern-action-git-stage)
    nmap <buffer><nowait>         >>      <Plug>(fern-action-git-unstage)
  endfunction

  augroup fern-settings
    autocmd!
    autocmd FileType fern call s:SetupFern()
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

function s:PlugMisc()
  Plug 'tpope/vim-eunuch'             " UNIX-like functionality in Vim
  Plug 'ojroques/vim-oscyank'         " Yank across the terminal
    augroup osc-yank
      autocmd!
      autocmd TextYankPost * if v:event.operator is 'y' && v:event.regname is '' | execute 'OSCYankReg "' | endif
    augroup END
  Plug 'farmergreg/vim-lastplace'     " Open where last opened
  Plug 'duggiefresh/vim-easydir'      " Create directories when non-existent
  Plug 'strboul/urlview.vim'          " See all URLs in buffer
  Plug 'chrisbra/Recover.vim'         " See diff for recover
  Plug 'lervag/file-line'             " Open file:line
  Plug 'lambdalisue/suda.vim'         " Give vim sudo powers
    let g:suda_smart_edit = 1
  return []
endfunction

function plugins#files#setup()
  let l:callbacks = []
  let l:callbacks += s:PlugMisc()
  " let l:callbacks += s:PlugVinegar()
  " let l:callbacks += s:PlugDirvish()
  let l:callbacks += s:PlugFern()
  let l:callbacks += s:PlugRooter()
  return l:callbacks
endfunction
