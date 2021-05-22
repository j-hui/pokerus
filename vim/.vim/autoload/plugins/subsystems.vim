
function plugins#subsystems#setup()

  Plug 'mbbill/undotree'
  " See undo history
    nnoremap <C-w>u :UndotreeToggle<cr>:UndotreeFocus<cr>

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

    function! RipgrepFzf(query, fullscreen)
      let command_fmt = 'rg --column --line-number --no-heading --color=always --smart-case -- %s || true'
      let initial_command = printf(command_fmt, shellescape(a:query))
      let reload_command = printf(command_fmt, '{q}')
      let spec = {'options': ['--phony', '--query', a:query, '--bind', 'change:reload:'.reload_command]}
      call fzf#vim#grep(initial_command, 1, fzf#vim#with_preview(spec), a:fullscreen)
    endfunction

    " Insert mode completion (overriding vim mappings)
    imap <C-x><C-k> <plug>(fzf-complete-word)
    imap <C-x><C-f> <plug>(fzf-complete-path)
    imap <C-x><C-l> <plug>(fzf-complete-line)

    command! FZHere call fzf#run(fzf#wrap({'dir': expand('%:h')}))

    " Normal mode mappings (with mnemonics)

      " Ripgrep (under cursor)
      nmap <C-x>r :Rg<CR>

      " Ripgrep live (starting with word under cursor)
      nmap <C-x>R :call RipgrepFzf(expand('<cword>'), 0)<CR>

      " Files
      nmap <C-x>f :Files<CR>

      " Git files
      nmap <C-x>F :GFiles<CR>

      " Here (like Files/:FZF, but relative to directory of current file)
      nmap <C-x>H :call fzf#run(fzf#wrap({'dir': expand('%:h')}))<CR>

      " Git commits (current buffer only)
      nmap <C-x>g :BCommits<CR>

      " Git commits
      nmap <C-x>G :Commits<CR>

      " Buffers (and history)
      nmap <C-x>h :History<CR>

      " Buffers
      nmap <C-x>b :Buffers<CR>

      " Lines (current buffer only)
      nmap <C-x>l :BLines<CR>

      " Lines
      nmap <C-x>L :Lines<CR>

      " Command history
      nmap <C-x>: :History:<CR>
      nmap <C-x>; :History:<CR>

      " Search history
      nmap <C-x>/ :History/<CR>

  Plug 'https://gitlab.com/mcepl/vim-fzfspell.git'
  " FZF for z=

  Plug 'junegunn/vim-peekaboo'
  " See yank registers

  Plug 'tpope/vim-fugitive'
  " Git interaction
    command! Gd Gdiffsplit
    command! GD Gdiffsplit

  Plug 'rhysd/git-messenger.vim'
  " Super-charged git blame
    let g:git_messenger_no_default_mappings = v:true
    nmap gb <Plug>(git-messenger)

  Plug 'preservim/tagbar'
  " Outline by tags

  Plug 'cosminadrianpopescu/vim-tail'
  " Make vim behave like tail -f

  Plug 'itchyny/calendar.vim'
  " Calendar app in Vim

  return []
endfunction
