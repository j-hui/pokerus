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
      let g:which_key_map_space = g:which_key_map[' '] " i.e., <leader>
      call which_key#register('<Space>', 'g:which_key_map_space')
    endfunction
  return [function('s:WhichKeyHooks')]
endfunction

function s:StackWhichKeyNvim()
  Plug 'folke/which-key.nvim'

  function s:SetupWhichKeyNvim()
lua << EOF
    local wk = require"which-key"
    wk.setup { hidden = { -- hide mapping boilerplate
      "<silent>",
      "<cmd>",
      "<Cmd>",
      "<CR>",
      "call",
      "lua",
      "^:",
      "^ ",
      "<Plug>",
      "<plug>"
    }}
    wk.register(vim.g.which_key_map, { mode = "n" })
    wk.register(vim.g.which_key_map_v, { mode = "v" })
    wk.register(vim.g.which_key_map_i, { mode = "i" })
EOF
  endfunction

  augroup SetupWhichKey
  autocmd!
  autocmd VimEnter * call s:SetupWhichKeyNvim()
  augroup END

  return []
endfunction

function s:PlugFzf()
  Plug 'junegunn/fzf.vim'
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  " Fuzzy finder
    let g:fzf_preview_window = 'right:60%'
  Plug 'https://gitlab.com/mcepl/vim-fzfspell.git'
  " FZF for z=

  " Shortcut for History: and History/
  command! H History
  return []
endfunction

function s:StackFzf()
  call s:PlugFzf()

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

  " Insert mode completion (overriding vim mappings)
  imap <C-x><C-k> <Plug>(fzf-complete-word)
  imap <C-x><C-f> <Plug>(fzf-complete-path)
  imap <C-x><C-l> <Plug>(fzf-complete-line)

  " Here (like Files/:FZF, but relative to directory of current file)
  nmap <leader>. :call FzfHere(0)<CR>
  call g:WhichKeyL(['.'], 'fzf-here')

  " Lines (current buffer only)
  nmap <leader>, :BLines<CR>
  call g:WhichKeyL([','], 'fzf-buffer-lines')

  " Line
  nmap <leader>s :Lines<CR>
  call g:WhichKeyL(['/'], 'fzf-lines')

  " Ripgrep live (starting with word under cursor)
  nmap <leader>/ :call FzfRg(expand('<cword>'), 0)<CR>
  call g:WhichKeyL(['?'], 'fzf-ripgrep')

  " Files
  nmap <leader>f :Files<CR>
  call g:WhichKeyL(['f'], 'fzf-files')

  " Switch to buffer
  nmap <leader>b :Buffers<CR>
  call g:WhichKeyL(['b'], 'fzf-buffers')

  " Switch to buffer + history
  nmap <leader>o :History<CR>
  call g:WhichKeyL(['s'], 'fzf-history-buffers')

  nmap <leader>g. :BCommits<CR>
  call g:WhichKeyL(['g', '.'], 'git-buffer-log')

  nmap <leader>gl :Commits<CR>
  call g:WhichKeyL(['g', 'l'], 'git-log')

  nmap <leader>gg :GFiles<CR>
  call g:WhichKeyL(['g', 'g'], 'git-files')

  return []
endfunction

function s:PlugTelescope()
  Plug 'nvim-lua/plenary.nvim'
  Plug 'nvim-telescope/telescope.nvim'
  Plug 'nvim-telescope/telescope-file-browser.nvim'
  Plug 'tami5/sqlite.lua'
  Plug 'ahmedkhalf/project.nvim'
  Plug 'nvim-telescope/telescope-fzf-native.nvim', { 'do': 'make' }
  return []
endfunction

function s:StackTelescope()
  call s:PlugTelescope()

  function s:SetupTelescope()
lua << EOF
    local function action(f, ...)
      local args = {...}
      return function (b)
        require"telescope.actions"[f](b, unpack(args))
      end
    end
    local function action_set(f, ...)
      local args = {...}
      return function (b)
        require"telescope.actions.set"[f](b, unpack(args))
      end
    end
    local function fb_action(f, ...)
      local args = {...}
      return function (b)
        require"telescope".extensions.file_browser.actions[f](b, unpack(args))
      end
    end
    local imaps = {
      ["<C-a>"] = { "<Home>",   type = "command" },
      ["<C-e>"] = { "<End>",    type = "command" },
      ["<C-f>"] = { "<Right>",  type = "command" },
      ["<C-b>"] = { "<Left>",   type = "command" },
      ["<C-h>"] = { "<BS>",     type = "command" },
      ["<C-k>"] = function () vim.cmd "norm! d$" end,
      ["<Esc>"] = action "close",
    }
    local nmaps = {
      ["<Up>"]    = { "k",      type = "command" },
      ["<Down>"]  = { "j",      type = "command" },
      ["<Left>"]  = { "h",      type = "command" },
      ["<Right>"] = { "l",      type = "command" },
      ["q"]       = action "close",
      ["<C-u>"]   = action_set("shift_selection", -12),
      ["<C-d>"]   = action_set("shift_selection", 12),
      ["<C-p>"]   = action_set("shift_selection", -1),
      ["<C-n>"]   = action_set("shift_selection", 1),
    }
    require"telescope".setup {
      defaults = require "telescope.themes".get_ivy {
        winblend = 20,
        mappings = { i = imaps, n = nmaps },
      },
      extensions = {
        file_browser = {
          mappings = {
            n = nmaps,
            i = vim.tbl_extend("force", imaps, {
              ["<C-y>"] = fb_action "create",
              ["<C-t>"] = fb_action "toggle_browser",
            }),
          },
        },
      },
    }
    require"project_nvim".setup {}
    require"telescope".load_extension "file_browser"
    require"telescope".load_extension "projects"
    require"telescope".load_extension "fzf"

EOF

    nnoremap <leader>f <cmd>lua require"telescope.builtin".find_files{}<CR>
    call g:WhichKeyL(['f'], 'telescope-files')

    nnoremap <leader>. <cmd>lua require"telescope.builtin".find_files{cwd = require"telescope.utils".buffer_dir()}<CR>
    call g:WhichKeyL(['.'], 'telescope-here')

    " nnoremap <leader>. <cmd>lua require"telescope".extensions.file_browser.file_browser{}<CR>
    " call g:WhichKeyL(['.'], 'telescope-file-browser')
    " I have other file browser plugins for this

    nnoremap <leader>b <cmd>lua require"telescope.builtin".buffers{}<CR>
    call g:WhichKeyL(['b'], 'telescope-buffers')

    nnoremap <leader>* <cmd>lua require"telescope.builtin".grep_string{}<CR>
    call g:WhichKeyL(['*'], 'telescope-grep-word')

    nnoremap <leader>/ <cmd>lua require"telescope.builtin".live_grep{}<CR>
    call g:WhichKeyL(['/'], 'telescope-search')

    nnoremap <leader>s <cmd>lua require"telescope.builtin".live_grep{grep_open_files = true}<CR>
    call g:WhichKeyL(['s'], 'telescope-sneak')

    nnoremap <leader>g. <cmd>lua require"telescope.builtin".git_bcommits{}<CR>
    call g:WhichKeyL(['g', '.'], 'git-buffer-log')

    nnoremap <leader>gl <cmd>lua require"telescope.builtin".git_commits{}<CR>
    call g:WhichKeyL(['g', 'l'], 'git-log')

    nnoremap <leader>gg <cmd>lua require"telescope.builtin".git_files{}<CR>
    call g:WhichKeyL(['g', 'g'], 'git-files')

    command! H           :Telescope help_tags<CR>
    command! Help        :Telescope help_tags<CR>

    command! Hi          :Telescope highlights<CR>
    command! Highlights  :Telescope highlights<CR>

    command! M           :Telescope man_pages<CR>
    command! Manpages    :Telescope man_pages<CR>
  endfunction
  return [function('s:SetupTelescope')]
endfunction

function s:StackGit()
  let l:callbacks = []

  Plug 'tpope/vim-fugitive'
  Plug 'tpope/vim-rhubarb'
  " Git interaction
    call g:WhichKeyL(['g', 'name'], '+git')

    nnoremap <leader>gd :Gdiffsplit<CR>
    call g:WhichKeyL(['g', 'd'], 'git-diff-split')
    nnoremap <leader>gD :Git diff --cached<CR>
    call g:WhichKeyL(['g', 'D'], 'git-diff-cached')
    nnoremap <leader>gp :Git pull<CR>
    call g:WhichKeyL(['g', 'p'], 'git-pull')
    nnoremap <leader>gP :Git push<CR>
    call g:WhichKeyL(['g', 'P'], 'git-push')
    nnoremap <leader>gc :Git commit<CR>
    call g:WhichKeyL(['g', 'c'], 'git-commit')
    nnoremap <leader>gs :Git<CR>
    call g:WhichKeyL(['g', 's'], 'git-status')
    nnoremap <leader>gw :Gw<CR>
    call g:WhichKeyL(['g', 'w'], 'git-write')

    " nnoremap <leader>gL :Gclog<CR>
    " " -- Use :Flogsplit instead

    nmap <leader>gb :GBrowse<CR>
    call g:WhichKeyL(['g', 'b'], 'git-browse')

  Plug 'rbong/vim-flog'
  " Git log
    nnoremap <leader>gL :Flogsplit<CR>
    call g:WhichKeyL(['g', 'L'], 'git-flog')

    augroup fugitive_maps
      autocmd!
      autocmd Filetype fugitive,git,floggraph nmap <buffer> q gq
    augroup END

  Plug 'rhysd/git-messenger.vim'
  " Super-charged git blame
    let g:git_messenger_no_default_mappings = v:true
    nmap <leader>gb <Plug>(git-messenger)
    call g:WhichKeyL(['g', 'b'], 'git-blame')

  if has('nvim-0.5')
    Plug 'ruifm/gitlinker.nvim'
    " Yank link to web-hosted git page

    function s:SetupGitLinker()
lua <<EOF
      require'gitlinker'.setup{
        opts = {
          action_callback = function(url)
            vim.api.nvim_command('let @" = \'' .. url .. '\'')
            vim.fn.OSCYankString(url)
          end,
        },
      }
EOF
      call g:WhichKeyL(['g', 'y'], 'git-url')
    endfunction
    let l:callbacks += [function('s:SetupGitLinker')]
  endif

  return l:callbacks
endfunction

function s:StackMiscSubsystems()
  let l:callbacks = []

  Plug 'mbbill/undotree'
  " See undo history
    nnoremap <leader>u :UndotreeToggle<cr>:UndotreeFocus<cr>
    call g:WhichKeyL(['u'], 'undotree-toggle')

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
    function s:SetupCalendar()
      if filereadable(expand('~/.local/share/vim/calendar.vim'))
        source ~/.local/share/vim/calendar.vim
      endif
    endfunction
    let l:callbacks += [function('s:SetupCalendar')]

  Plug 'Shougo/vimproc', { 'do': 'make' }
  Plug 'yuratomo/gmail.vim'
    function s:SetupGmail()
      if filereadable(expand('~/.local/share/vim/gmail.vim'))
        source ~/.local/share/vim/gmail.vim
      endif
    endfunction
    let l:callbacks += [function('s:SetupGmail')]

  Plug 'kassio/neoterm'
  " Send commands to terminal
    let g:neoterm_default_mod = 'botright' " Open new terminal in split window

    nmap <leader>; :T<space>
    call g:WhichKeyL([';'], 'neoterm-cmd')
    nmap <leader>t :Ttoggle<CR>
    call g:WhichKeyL(['t'], 'neoterm-toggle')

    nmap g; <Plug>(neoterm-repl-send)
    xmap g; <Plug>(neoterm-repl-send)
    nmap g;; <Plug>(neoterm-repl-send-line)

    let g:neoterm_automap_keys = 'g:'

  Plug 'mhinz/vim-startify'
  " Start screen for vim

  let g:startify_padding_left = max([16, &columns / 4])
  let g:vim_logo = [
        \ '  ##############..... ##############   ',
        \ '  ##############......##############   ',
        \ '    ##########..........##########     ',
        \ '    ##########........##########       ',
        \ '    ##########.......##########        ',
        \ '    ##########.....##########..        ',
        \ '    ##########....##########.....      ',
        \ '  ..##########..##########.........    ',
        \ '....##########.#########.............  ',
        \ '  ..################JJJ............    ',
        \ '    ################.............      ',
        \ '    ##############.JJJ.JJJJJJJJJJ      ',
        \ '    ############...JJ...JJ..JJ  JJ     ',
        \ '    ##########....JJ...JJ..JJ  JJ      ',
        \ '    ########......JJJ..JJJ JJJ JJJ     ',
        \ '    ######    .........                ',
        \ '                .....                  ',
        \ '                  .                    ',
        \]
  let g:startify_custom_header = 'startify#center(g:vim_logo)'

  if has('nvim-0.5')
    Plug 'chentau/marks.nvim'
    " Show marks in sign column

      function s:SetupMarksNvim()
lua <<EOF
        require'marks'.setup {}
EOF
      endfunction

      let l:callbacks += [function('s:SetupMarksNvim')]

    Plug 'stevearc/stickybuf.nvim'
      " Prevent files from being opened in pop-up tray windows
  endif

  return l:callbacks
endfunction

function s:StackDressing()
  Plug 'stevearc/dressing.nvim'
    " Extend vim.ui hooks
  return []
endfunction

function plugins#subsystems#setup()
  let l:callbacks = []
  if has('nvim-0.5')
    let l:callbacks += s:StackWhichKeyNvim()
  else
    let l:callbacks += s:StackWhichKey()
  endif

  if has('nvim-0.6')
    let l:callbacks += s:PlugFzf()
    let l:callbacks += s:StackTelescope()
  else
    let l:callbacks += s:StackFzf()
  endif

  if has('nvim-0.6')
    let l:callbacks += s:StackDressing()
  endif

  let l:callbacks += s:StackGit()
  let l:callbacks += s:StackMiscSubsystems()
  return l:callbacks
endfunction
