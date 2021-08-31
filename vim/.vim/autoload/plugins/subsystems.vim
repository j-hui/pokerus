
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
      function! s:FzfFileAccept(lines) abort
        " a:lines is a list with three elements (two if there were no matches):
        "   <search-query>, <expect-key>|<empty> [, <selected-items...>]
        if len(a:lines) < 2
          return
        elseif len(a:lines) == 2 || !empty(a:lines[1]) |
          execute 'edit ' . a:lines[0]
        else
          execute 'edit ' . split(a:lines[2], '#####')[0]
        endif
      endfunction

      let l:spec = {
            \'options': ['-d=#####', '--print-query', '--expect=ctrl-j'],
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

  Plug 'junegunn/vim-peekaboo'
  " See yank registers

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  " Git interaction
    let g:which_key_map['g'] = { 'name': '+git' }

    nmap <leader>gg :GFiles<CR>
    let g:which_key_map['g']['e'] = 'git-fzf-files'

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
