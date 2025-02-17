function! s:readlineMaps() abort
  " NOTE: some readline-style bindings cherrypicked/simplified from tpope/vim-rsi

  if &encoding ==# 'latin1' && has('gui_running') && !empty(findfile('plugin/sensible.vim', escape(&runtimepath, ' ')))
    set encoding=utf-8
  endif

  function! s:MapMeta() abort
    noremap!    <M-d> <C-O>dw
    cnoremap    <M-d> <S-Right><C-W>
    noremap!    <M-n> <Down>
    noremap!    <M-p> <Up>
    noremap!    <M-BS> <C-W>
    noremap!    <M-C-h> <C-W>
  endfunction

  if has('gui_running') || has('nvim')
    call s:MapMeta()
  else
    silent! exe "set <F29>=\<Esc>b"
    silent! exe "set <F30>=\<Esc>f"
    silent! exe "set <F31>=\<Esc>d"
    silent! exe "set <F32>=\<Esc>n"
    silent! exe "set <F33>=\<Esc>p"
    silent! exe "set <F34>=\<Esc>\<C-?>"
    silent! exe "set <F35>=\<Esc>\<C-H>"
    noremap!    <F29> <S-Left>
    noremap!    <F30> <S-Right>
    noremap!    <F31> <C-O>dw
    cnoremap    <F31> <S-Right><C-W>
    noremap!    <F32> <Down>
    noremap!    <F33> <Up>
    noremap!    <F34> <C-W>
    noremap!    <F35> <C-W>
    augroup rsi_gui
    autocmd GUIEnter * call s:MapMeta()
    augroup END
  endif
endfunction

function! s:navigationMaps() abort
  " Move forward by a character
  nnoremap g<Space> <Space>

  " Block-oriented (non-linewise) navigation
  nnoremap j gj
  nnoremap k gk

  " Get out insert mode easily
  inoremap <C-c>  <Esc>
  " inoremap kj   <Esc>

  " center cursorline when scrolling
  nnoremap <C-d>  <C-d>zz
  nnoremap <C-u>  <C-u>zz

  " clear screen and highlights with <C-l>
  nnoremap <C-l>  <cmd>noh<cr><C-l>
  inoremap <C-l>  <cmd>noh<cr><C-o><C-l>

  " Normal mode readline style navigation
  nnoremap <C-n>    <C-e>j
  nnoremap <C-p>    <C-y>k
  nnoremap <C-e>    $
  nnoremap <C-a>    ^
  nnoremap <C-f>    l
  nnoremap <C-b>    h

  " Virual mode readline style navigation
  vnoremap <C-n>    <C-e>j
  vnoremap <C-p>    <C-y>k
  vnoremap <C-e>    $
  vnoremap <C-a>    ^
  vnoremap <C-f>    l
  vnoremap <C-b>    h

  " Window navigation
  noremap <C-w>s <C-w>v
  noremap <C-w>x <C-w>s

  inoremap <C-F> <Right>
  inoremap <C-B> <Left>
  " inoremap   <C-X><C-A> <C-A>
  inoremap <C-A> <C-O>^

  inoremap    <C-n> <C-o>gj
  inoremap    <C-p> <C-o>gk

  nnoremap <silent> = :let @/='\<'.expand('<cword>').'\>'<bar>set hlsearch<CR>

  function! s:i_ctrl_e() abort
    " If in context menu, accept selection
    if pumvisible()
      return "\<C-y>"
    endif
    if col('.') > strlen(getline('.'))
      return "\<C-e>" " if at line end, fallback to default <C-e> behavior
    endif
    " Otherwise, go to <End> of line
    return "\<End>"
  endfunction

  inoremap <expr> <C-e> <SID>i_ctrl_e()

  " If we are already at the end of the line, fall back to default <C-e> behavior (insert character from next line)
  " inoremap <expr> <C-e> col('.')>strlen(getline('.'))<bar><bar>pumvisible()?"\<Lt>C-y>":"\<Lt>End>"

  " I don't really use default bindings for <C-f> or <C-b>
  " inoremap <expr> <C-F> col('.')>strlen(getline('.'))?"\<Lt>C-F>":"\<Lt>Right>"
  " inoremap <expr> <C-B> getline('.')=~'^\s*$'&&col('.')>strlen(getline('.'))?"0\<Lt>C-D>\<Lt>Esc>kJs":"\<Lt>Left>"

  " Wrangle folds from jumping
  nnoremap Q za

  " Open parent directory of current buffer
  nmap <silent> g- :edit %:h<CR>

  " Open current working directory
  nmap <silent> g= :edit .<CR>
endfunction

function! s:editingMaps() abort
  set pastetoggle=<F2>
  " Don't leave visual mode when indending
  xnoremap < <gv
  xnoremap > >gv

  " Current filename w/o extension
  inoremap <C-g>f <C-r>=expand('%:t:r')<CR>

  " Current filename w/ extension
  inoremap <C-g>F <C-r>=expand('%:t')<CR>

  " Correct spelling
  inoremap <C-g>s <c-g>u<Esc>[s1z=`]a<c-g>u

  " Insert date/time
  inoremap <C-g>d <C-R>=strftime("%Y-%m-%d")<CR>
  inoremap <C-g>t <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>

  " Auto format
  inoremap <silent> <C-g>e <C-o>gqq<C-o>$
  inoremap <silent> <C-g>q <C-o>gqq

  " Auto indentation
  inoremap <C-g><C-g> <C-F>

  " For readline-style line deletion, we use the delete register "-
  nnoremap <C-k> D
  inoremap <C-k> <C-o>D

  " inoremap <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"

  inoremap <C-q> <C-o>v

  nnoremap _ "_

  nnoremap x "_x
  nnoremap X "_X
  nnoremap D "_D
  nnoremap c "_c
  nnoremap cc "_S
  nnoremap C "_C

  xnoremap x "_x
  xnoremap X "_X
  xnoremap D "_D
  xnoremap c "_c
  xnoremap C "_C
endfunction

function! s:commandMaps() abort
  " Eliminate extra key press
  nnoremap ; :
  vnoremap ; :

  " Alternative mapping for :
  nnoremap <Space>; :
  vnoremap <Space>; :

  " Don't use <Left> and <Right> key for selecting previous/next match
  cnoremap <Left> <Space><BS><Left>
  cnoremap <Right> <Space><BS><Right>

  cnoremap <C-F> <Right>
  cnoremap <C-B> <Left>

  " cnoremap   <C-X><C-A> <C-A>
  cnoremap   <C-A> <Home>
  cnoremap <expr> <C-E> col('.')>strlen(getline('.'))?"\<Lt>C-E>":"\<Lt>End>"
  " Note that <C-e> also "steps into" wild menu

  " cnoremap <expr> <C-D> getcmdpos()>strlen(getcmdline())?"\<Lt>C-D>":"\<Lt>Del>"
  " cnoremap <expr> <C-F> getcmdpos()>strlen(getcmdline())?&cedit:"\<Lt>Right>"

  function! s:c_ctrl_u() abort
    " If in context menu, simulate page up-ish behavior
    if pumvisible()
      return repeat("\<c-p>", 10)
    endif

    " Otherwise, delete to beginning of line, stash contents in small delete register
    if getcmdpos() > 1
      let @- = getcmdline()[:getcmdpos()-2]
    endif
    return "\<C-U>"
  endfunction

  function! s:c_ctrl_d() abort
    " If in context menu, simulate page down-ish behavior
    if pumvisible()
      return repeat("\<c-n>", 10)
    endif
    " Otherwise, simulate delete
    return "\<Del>"
  endfunction

  function! s:c_ctrl_k() abort
    " Simulate kill line, stash contents in small delete register
    if getcmdpos() > 0
      let @- = getcmdline()[getcmdpos()-1:]
    endif
    return "\<C-\>e(strpart(getcmdline(), 0, getcmdpos() - 1))\<CR>"
  endfunction

  cnoremap <expr> <C-u> <SID>c_ctrl_u()
  cnoremap <expr> <C-d> <SID>c_ctrl_d()
  cnoremap <expr> <C-k> <SID>c_ctrl_k()

  " Yank from delete register
  cnoremap <C-Y> <C-R>-
endfunction

function! s:terminalMaps() abort
  tnoremap <C-]> <C-\><C-N>
  tnoremap <C-\>h <Cmd>wincmd h<CR>
  tnoremap <C-\>l <Cmd>wincmd l<CR>
  tnoremap <C-\>j <Cmd>wincmd j<CR>
  tnoremap <C-\>k <Cmd>wincmd k<CR>
  tnoremap <C-\>H <Cmd>wincmd H<CR>
  tnoremap <C-\>L <Cmd>wincmd L<CR>
  tnoremap <C-\>J <Cmd>wincmd J<CR>
  tnoremap <C-\>K <Cmd>wincmd K<CR>
  tnoremap <C-\>o <Cmd>wincmd o<CR>
  tnoremap <C-\>r <Cmd>wincmd r<CR>
  tnoremap <C-\>: <C-\><C-N>:
  tnoremap <C-\>; <C-\><C-N>:
  tnoremap <C-\>v <Cmd>wincmd v<CR>
  tnoremap <C-\>s <Cmd>wincmd s<CR>
endfunction

function! s:windowMaps() abort
  nnoremap doq <cmd>cclose<CR>
  nnoremap dol <cmd>lclose<CR>
  nnoremap doh <cmd>helpclose<CR>
endfunction

function! pokerus#keybinds#setup() abort

  " Disable ex mode
  nnoremap Q <nop>
  nnoremap gQ <nop>

  call s:readlineMaps()
  call s:navigationMaps()
  call s:editingMaps()
  call s:commandMaps()
  call s:terminalMaps()
  call s:windowMaps()

endfunction

" vim: set ts=2 sw=2 tw=80 et foldmethod=syntax :
