
function s:readlineMaps()
  " NOTE: some readline-style bindings cherrypicked/simplified from tpope/vim-rsi

  if &encoding ==# 'latin1' && has('gui_running') && !empty(findfile('plugin/sensible.vim', escape(&rtp, ' ')))
    set encoding=utf-8
  endif

  function! s:MapMeta() abort
    noremap!    <M-b> <S-Left>
    noremap!    <M-f> <S-Right>
    noremap!    <M-d> <C-O>dw
    cnoremap    <M-d> <S-Right><C-W>
    noremap!    <M-n> <Down>
    noremap!    <M-p> <Up>
    noremap!    <M-BS> <C-W>
    noremap!    <M-C-h> <C-W>
  endfunction

  if has("gui_running") || has('nvim')
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

function s:navigationMaps()
  " Block-oriented (non-linewise) navigation
  nnoremap j gj
  nnoremap k gk

  " Get out insert mode easily
  inoremap <C-c>  <Esc>
  " inoremap kj   <Esc>

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
  noremap <C-w>n <Esc>:bn<CR>
  noremap <C-w>p <Esc>:bp<CR>
  noremap <C-w>q <Esc>:bd<CR>

  inoremap <C-F> <Right>
  inoremap <C-B> <Left>
  " inoremap   <C-X><C-A> <C-A>
  inoremap <C-A> <C-O>^

  inoremap    <C-n> <down>
  inoremap    <C-p> <up>

  function! s:i_ctrl_e()
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
endfunction

function s:editingMaps()
  set pastetoggle=<F2>
  " Don't leave visual mode when indending
  xnoremap < <gv
  xnoremap > >gv

  " Correct spelling
  inoremap <C-g>l <c-g>u<Esc>[s1z=`]a<c-g>u

  " Insert date/time
  inoremap <C-g>d <C-R>=strftime("%Y-%m-%d")<CR>
  inoremap <C-g>t <C-R>=strftime("%Y-%m-%d %H:%M:%S")<CR>

  " Auto format
  inoremap <silent> <C-g>e <C-o>gqq<C-o>$
  inoremap <silent> <C-g>q <C-o>gqq

  " Auto indentation
  inoremap <C-g><C-g> <C-F>

  " For readline-style line deletion, we use the delete register "-
  nnoremap <C-k> "-D
  inoremap <C-k> <C-o>"-D
  nnoremap <C-y> "-p
  inoremap <C-y> <C-o>"-p

  " inoremap <expr> <C-D> col('.')>strlen(getline('.'))?"\<Lt>C-D>":"\<Lt>Del>"
endfunction

function s:commandMaps()
  " Eliminate extra key press
  nnoremap ; :
  vnoremap ; :

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

  function! s:c_ctrl_u()
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

  function! s:c_ctrl_d()
    " If in context menu, simulate page down-ish behavior
    if pumvisible()
      return repeat("\<c-n>", 10)
    endif
    " Otherwise, simulate delete
    return "\<Del>"
  endfunction

  function! s:c_ctrl_k()
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

function keybinds#setup()

  " Disable ex mode
  nnoremap Q <nop>
  nnoremap gQ <nop>

  call s:readlineMaps()
  call s:navigationMaps()
  call s:editingMaps()
  call s:commandMaps()

endfunction

" vim: set ts=2 sw=2 tw=80 et foldmethod=syntax :
