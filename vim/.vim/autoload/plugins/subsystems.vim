function s:StackWhichKey()
  Plug 'liuchengxu/vim-which-key'
    let g:which_key_display_names = {
          \ '<CR>': '↵',
          \ '<TAB>': '⇆',
          \ '<BS>': '⌫',
          \ '<Space>': '␣',
          \}

    nnoremap <silent> <leader> :WhichKey '<Space>'<CR>

    function s:WhichKeyHooks()
      call which_key#register('<Space>', "g:which_key_map")
    endfunction

    " Defined for s:leaderMaps
    let g:which_key_map['b'] = { 'name': '+buffer-window' }
    let g:which_key_map['b']['n'] = 'buffer-next'
    let g:which_key_map['b']['p'] = 'buffer-previous'
    let g:which_key_map['b']['w'] = 'buffer-wipe'
    let g:which_key_map['b']['q'] = 'window-quit'
    let g:which_key_map['b']['j'] = 'window-down'
    let g:which_key_map['b']['k'] = 'window-up'
    let g:which_key_map['b']['h'] = 'window-left'
    let g:which_key_map['b']['l'] = 'window-right'
    let g:which_key_map['b']['s'] = 'window-horizontal-split'
    let g:which_key_map['b']['v'] = 'window-vertical-split'
    let g:which_key_map['b']['c'] = 'buffer-close-lists'

  return [function('s:WhichKeyHooks')]
endfunction

function s:StackFzf()
  Plug 'junegunn/fzf.vim'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  " Fuzzy finder

    let g:fzf_preview_window = 'right:60%'

    " Redefine :Rg, but using word under cursor if no args are given
    command! -bang -nargs=* Rg
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(len(<q-args>)?<q-args>:expand('<cword>')), 1,
      \   fzf#vim#with_preview(), <bang>0)
    command! -bang -nargs=* RG
      \ call fzf#vim#grep(
      \   'rg --column --line-number --no-heading --color=always --smart-case -- '.shellescape(len(<q-args>)?<q-args>:expand('<cword>')), 1,
      \   fzf#vim#with_preview(), <bang>0)

    function! FzfRg(query, fullscreen)
      let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
      let initial_command = printf(command_fmt, shellescape(a:query))
      let reload_command = printf(command_fmt, '{q}')
      let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
      call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
    endfunction

    function! FzfHere(fullscreen)
      call fzf#vim#files(expand('%:h'), fzf#vim#with_preview(), a:fullscreen)
    endfunction

    function! FzfFiles(fullscreen)
      " The following commands can be used in the fzf window to select:
      " - return: open current selection
      " - ctrl-o: open current query (creates a file if non-existent)
      " - ctrl-v: open current query in vertical split
      " - ctrl-x: open current query in horizontal split
      "
      " The following commands can be used to interact with the fzf window:
      " - ctrl-l: replace query with current selection
      " - ctrl-u: scroll half page up
      " - ctrl-d: scroll half page down
      " - tab/ctrl-i: select
      function! s:FzfFileAccept(lines) abort
        " a:lines is a list with three elements (two if there were no matches):
        "   <search-query>, <expect-key>|<empty> [, <selected-items...>]
        if len(a:lines) < 2
          echoerr 'FzfFiles: truncated input: ' . a:lines
          return
        endif

        if empty(a:lines[1]) " Enter was pressed
          if len(a:lines) == 2
            " No matches; open query
            execute 'edit ' . a:lines[0]
          else
            " Open selection
            for l:selection in a:lines[2:]
              execute 'edit ' . l:selection
            endfor
          endif
        elseif a:lines[1] ==# 'ctrl-o'
          " Open query
          execute 'edit ' . a:lines[0]
        elseif a:lines[1] ==# 'ctrl-v'
          " Open query in vsplit
          execute 'vsplit ' . a:lines[0]
        elseif a:lines[1] ==# 'ctrl-x'
          " Open query in hsplit
          execute 'split ' . a:lines[0]
        else
          echoerr 'FzfFiles: unknown command ' . a:lines[1]
        endif

      endfunction

      let l:spec = {
            \'options': [
              \'--print-query',
              \'--expect=ctrl-o,ctrl-v,ctrl-x',
              \'--bind=ctrl-l:replace-query',
              \'--bind=ctrl-u:half-page-up',
              \'--bind=ctrl-d:half-page-down',
              \],
            \'sink*': funcref('s:FzfFileAccept')
            \}
      call fzf#vim#files(getcwd(), fzf#vim#with_preview(l:spec), a:fullscreen)
    endfunction

    " Insert mode completion (overriding vim mappings)
    imap <C-x><C-k> <plug>(fzf-complete-word)
    imap <C-x><C-f> <plug>(fzf-complete-path)
    imap <C-x><C-l> <plug>(fzf-complete-line)

    " Here (like Files/:FZF, but relative to directory of current file)
    nmap <leader>. :call FzfHere(0)<CR>
    let g:which_key_map['.'] = 'fzf-here'

    " Lines (current buffer only)
    nmap <leader>, :BLines<CR>
    let g:which_key_map[','] = 'fzf-buffer-lines'

    " Line
    nmap <leader>/ :Lines<CR>
    let g:which_key_map['/'] = 'fzf-lines'

    " Ripgrep live (starting with word under cursor)
    nmap <leader>? :call FzfRg(expand('<cword>'), 0)<CR>
    let g:which_key_map['?'] = 'fzf-ripgrep'

    " Files
    nmap <leader>f :call FzfFiles(0)<CR>
    let g:which_key_map['f'] = 'fzf-files'

    " Switch to buffer
    nmap <leader>s :Buffers<CR>
    let g:which_key_map['s'] = 'fzf-buffers'

    " Switch to buffer + history
    nmap <leader>S :History<CR>
    let g:which_key_map['S'] = 'fzf-history-buffers'

    " Shortcut for History: and History/
    command! H History

  Plug 'https://gitlab.com/mcepl/vim-fzfspell.git'
  " FZF for z=

  return []
endfunction

function s:StackGit()
  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  " Git interaction
    let g:which_key_map['g'] = { 'name': '+git' }

    nnoremap <leader>gd :Gdiffsplit<CR>
    let g:which_key_map['g']['d'] = 'git-diff-split'
    nnoremap <leader>gD :Git diff --cached<CR>
    let g:which_key_map['g']['D'] = 'git-diff-cached'
    nnoremap <leader>gp :Git pull<CR>
    let g:which_key_map['g']['p'] = 'git-pull'
    nnoremap <leader>gP :Git push<CR>
    let g:which_key_map['g']['P'] = 'git-push'
    nnoremap <leader>gc :Git commit<CR>
    let g:which_key_map['g']['c'] = 'git-commit'
    nnoremap <leader>gs :Git<CR>
    let g:which_key_map['g']['s'] = 'git-status'
    nnoremap <leader>gw :Gw<CR>
    let g:which_key_map['g']['w'] = 'git-write'

    " nnoremap <leader>gL :Gclog<CR>
    " let g:which_key_map['g']['L'] = 'git-log-classic' " -- Use :Flogsplit

    " Browse commits with fzf
    nmap <leader>g. :BCommits<CR>
    let g:which_key_map['g']['.'] = 'git-fzf-buffer-log'

    nmap <leader>gl :Commits<CR>
    let g:which_key_map['g']['l'] = 'git-fzf-log'

    nmap <leader>gg :GFiles<CR>
    let g:which_key_map['g']['g'] = 'git-fzf-files'

  Plug 'rbong/vim-flog'
  " Git log
    nnoremap <leader>gL :Flogsplit<CR>
    let g:which_key_map['g']['L'] = 'git-flog'

    augroup fugitive_maps
      autocmd!
      autocmd Filetype fugitive,git,floggraph nmap <buffer> q gq
    augroup END

  Plug 'rhysd/git-messenger.vim'
  " Super-charged git blame
    let g:git_messenger_no_default_mappings = v:true
    nmap <leader>gb <Plug>(git-messenger)
    let g:which_key_map['g']['b'] = 'git-blame'
  return []
endfunction

function s:StackMiscSubsystems()
  let l:callbacks = []

  Plug 'mbbill/undotree'
  " See undo history
    nnoremap <leader>u :UndotreeToggle<cr>:UndotreeFocus<cr>
    let g:which_key_map['u'] = 'undotree-toggle'

  if has('nvim-0.4')
    Plug 'tversteeg/registers.nvim', { 'branch': 'main' }
    " See yank registers like completion pop-up
  else
    Plug 'junegunn/vim-peekaboo'
    " See yank registers
  endif

  Plug 'preservim/tagbar'
  " Outline by tags

  Plug 'cosminadrianpopescu/vim-tail'
  " Make vim behave like tail -f

  Plug 'itchyny/calendar.vim'
  " Calendar app in Vim

  Plug 'kassio/neoterm'
  " Send commands to terminal
    let g:neoterm_default_mod = 'botright' " Open new terminal in split window

    let g:which_key_map['g'] = { 'name': '+git' }

    nmap <leader>; :T<space>
    let g:which_key_map[';'] = 'neoterm-cmd'
    nmap <leader>t :Ttoggle<CR>
    let g:which_key_map['t'] = 'neoterm-toggle'

    nmap g; <Plug>(neoterm-repl-send)
    xmap g; <Plug>(neoterm-repl-send)
    nmap g;; <Plug>(neoterm-repl-send-line)


  if has('nvim-0.5')
    Plug 'chentau/marks.nvim'
    " Show marks in sign column

      function s:SetupMarksNvim()
        lua require'marks'.setup {}
      endfunction

      let l:callbacks += [function('s:SetupMarksNvim')]

    Plug 'stevearc/stickybuf.nvim'
      " Prevent files from being opened in pop-up tray windows
  endif

  return l:callbacks
endfunction

function plugins#subsystems#setup()
  let l:callbacks = []
  let l:callbacks += s:StackWhichKey()
  let l:callbacks += s:StackFzf()
  let l:callbacks += s:StackGit()
  let l:callbacks += s:StackMiscSubsystems()
  return l:callbacks
endfunction
