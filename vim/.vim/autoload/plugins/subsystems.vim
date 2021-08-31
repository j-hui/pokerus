
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
      let g:which_key_map['f'] = { 'name': '+fzf' }

      " Ripgrep (under cursor)
      nmap <leader>fr :Rg<CR>
      let g:which_key_map['f']['r'] = 'fzf-ripgrep'

      " Ripgrep live (starting with word under cursor)
      nmap <leader>fR :call RipgrepFzf(expand('<cword>'), 0)<CR>
      let g:which_key_map['f']['R'] = 'fzf-ripgrep-live'

      " Files
      nmap <leader>fe :Files<CR>
      let g:which_key_map['f']['e'] = 'fzf-files'

      " Git files
      nmap <leader>fg :GFiles<CR>
      let g:which_key_map['f']['g'] = 'fzf-git-files'

      " Here (like Files/:FZF, but relative to directory of current file)
      nmap <leader>f. :call fzf#run(fzf#wrap({'dir': expand('%:h')}))<CR>
      let g:which_key_map['f']['.'] = 'fzf-files-here'

      " Buffers (and history)
      nmap <leader>fh :History<CR>
      let g:which_key_map['f']['h'] = 'fzf-history-buffers'

      " Buffers
      nmap <leader>fb :Buffers<CR>
      let g:which_key_map['f']['b'] = 'fzf-buffers'

      " Lines
      nmap <leader>fl :Lines<CR>
      let g:which_key_map['f']['l'] = 'fzf-lines'

      " Lines (current buffer only)
      nmap <leader>ff :BLines<CR>
      let g:which_key_map['f']['f'] = 'fzf-buffer-lines'

      " Command history
      nmap <leader>f: :History:<CR>
      let g:which_key_map['f'][':'] = 'fzf-history-:'
      nmap <leader>f; :History:<CR>
      let g:which_key_map['f'][';'] = 'fzf-history-:'

      " Search history
      nmap <leader>f/ :History/<CR>
      let g:which_key_map['f']['/'] = 'fzf-history-/'

  Plug 'https://gitlab.com/mcepl/vim-fzfspell.git'
  " FZF for z=

  Plug 'junegunn/vim-peekaboo'
  " See yank registers

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
    nnoremap <leader>gL :Gclog<CR>
    let g:which_key_map['g']['L'] = 'git-log-classic'

    " Browse commits with fzf
    nmap <leader>gh :BCommits<CR>
    let g:which_key_map['g']['h'] = 'git-fzf-buffer-log'
    nmap <leader>gl :Commits<CR>
    let g:which_key_map['g']['l'] = 'git-fzf-log'

  Plug 'rbong/vim-flog'
  " Git log
    nnoremap <leader>gf :Flogsplit<CR>
    let g:which_key_map['g']['f'] = 'git-flog-split'
    nnoremap <leader>gf :Flog<CR>
    let g:which_key_map['g']['F'] = 'git-flog'

    augroup fugitive_maps
      autocmd!
      autocmd Filetype fugitive,git nmap <buffer> q gq
    augroup END

  Plug 'rhysd/git-messenger.vim'
  " Super-charged git blame
    let g:git_messenger_no_default_mappings = v:true
    nmap <leader>gb <Plug>(git-messenger)
    let g:which_key_map['g']['b'] = 'git-blame'

  Plug 'preservim/tagbar'
  " Outline by tags

  Plug 'cosminadrianpopescu/vim-tail'
  " Make vim behave like tail -f

  Plug 'itchyny/calendar.vim'
  " Calendar app in Vim

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
