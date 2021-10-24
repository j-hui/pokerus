scriptencoding utf-8

function s:PlugNcm2()
  let g:completion_tool = 'ncm2'
  Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp'
    Plug 'ncm2/ncm2-bufword'                      " Complete from buffer
    Plug 'ncm2/ncm2-path'                         " Complete from path
    Plug 'ncm2/ncm2-markdown-subscope'            " Language subscope for markdown
    Plug 'ncm2/ncm2-ultisnips'                    " Snippets
    Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim' " Completion for Vim
    let g:ncm2#popup_delay = 69

    function! s:NcmEnable() abort
      try
        call ncm2#enable_for_buffer()
      catch /^Vim\%((\a\+)\)\=:E117/ " Undefined function
        echom 'ncm2 not yet installed'
      endtry
    endfunction

    augroup Ncm2Hook
      autocmd!
      autocmd FileType * call <SID>NcmEnable()
      autocmd User Ncm2PopupOpen set completeopt=noinsert,menuone,noselect
      autocmd User Ncm2PopupClose set completeopt=menu,preview
    augroup END

  return []
endfunction

function s:PlugDeoplete()
  let g:completion_tool = 'deoplete'
  if has('nvim')
    Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  else
    Plug 'Shougo/deoplete.nvim'
    Plug 'roxma/nvim-yarp'
    Plug 'roxma/vim-hug-neovim-rpc'
  endif

    Plug 'deoplete-plugins/deoplete-dictionary'
    " Auto-complete dictionary word

    let g:deoplete#enable_at_startup = 1

    function s:DeopleteHooks()
      " These might fail if deoplete is not yet installed
      call deoplete#custom#option('auto_complete_delay', 200)
      call deoplete#custom#option('smart_case', v:true)
      call deoplete#custom#option('auto_refresh_delay', 500)
      set completeopt-=preview
    endfunction
    return [function('s:DeopleteHooks')]
endfunction

function s:PlugAsyncomplete()
  let g:completion_tool = 'asyncomplete'
  Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-buffer.vim'
    Plug 'prabirshrestha/asyncomplete-file.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'andreypopp/asyncomplete-ale.vim'
    Plug 'prabirshrestha/asyncomplete-ultisnips.vim'

    function s:AsyncompleteHooks()
      try
        call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
        \ 'name': 'buffer',
        \ 'allowlist': ['*'],
        \ 'completor': function('asyncomplete#sources#buffer#completor'),
        \ 'config': {
        \    'max_buffer_size': 5000000,
        \  },
        \ }))
        call asyncomplete#register_source(asyncomplete#sources#file#get_source_options({
        \ 'name': 'file',
        \ 'allowlist': ['*'],
        \ 'completor': function('asyncomplete#sources#file#completor')
        \ }))
        call asyncomplete#register_source(asyncomplete#sources#ultisnips#get_source_options({
        \ 'name': 'ultisnips',
        \ 'allowlist': ['*'],
        \ 'completor': function('asyncomplete#sources#ultisnips#completor'),
        \ }))
      catch /^Vim\%((\a\+)\)\=:E117/ " Undefined function
        echom 'Completion tool asyncomplete not yet installed'
      catch /^Vim\%((\a\+)\)\=:E121/ " Undefined variable
        echom 'Completion tool asyncomplete not yet installed'
      endtry
    endfunction

  return [function('s:AsyncompleteHooks')]
endfunction

function s:PlugLcn()
  Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }

    let g:LanguageClient_loggingLevel = 'INFO'
    let g:LanguageClient_diagnosticsList = 'Quickfix'
    let g:LanguageClient_settingsPath = expand('~/.config/nvim/settings.json')
    let g:LanguageClient_loggingFile = expand('~/.config/nvim/LanguageClient.log')
    let g:LanguageClient_serverCommands = {
      \ 'rust': ['rust-analyzer'],
      \ 'go': ['gopls'],
      \ 'c': ['ccls'],
      \ 'scala': ['metals'],
      \ 'cpp': ['ccls'],
      \ 'tex': ['texlab'],
      \ 'bib': ['texlab'],
      \ 'haskell': ['haskell-language-server-wrapper', '--lsp'],
      \ 'yaml': ['yaml-language-server', '--stdio'],
      \ 'python': ['pyright-langserver', '--stdio'],
      \ 'ocaml': ['ocamllsp'],
      \ 'vim': ['vim-language-server', '--stdio'],
      \ 'zig': ['zls'],
      \}
      " \ 'nix': ['rnix-lsp'],

    let g:LanguageClient_rootMarkers = {
      \ 'zig': ['build.zig'],
      \ 'vim': ['.git', 'autoload', 'plugin'],
      \}

    let g:LanguageClient_virtualTextPrefix = ' » '
    let g:LanguageClient_useVirtualText = 'All'
    let g:LanguageClient_hoverPreview = 'Always'

    let g:LanguageClient_codeLensDisplay = {
          \ 'virtualTexthl': 'Comment',
          \}
    let g:LanguageClient_enableExtensions = {
          \ 'go': v:true,
          \ 'rust': v:true,
          \}

    function! LcnFzfSelectionUI(source, sink) abort
      return fzf#run(fzf#wrap(fzf#vim#with_preview({
            \'source': a:source,
            \'sink': a:sink,
            \'options': ['--delimiter', ':', '--preview-window', '+{2}-5'],
            \})))
    endfunction

    let g:LanguageClient_selectionUI = function('LcnFzfSelectionUI')

    function s:LcnStarted()
      set signcolumn=yes
    " " Taken from https://github.com/autozimu/LanguageClient-neovim/issues/1095
    " let g:LanguageClient_fzfOptions =
    "       \ ['--delimiter', ':', '--preview-window', '+{2}-5'] +
    "       \ fzf#vim#with_preview().options

    endfunction

    function s:LcnStopped()
      set signcolumn=auto
    endfunction

    function s:LcnDoc()
      if index(['vim','help'], &filetype) >= 0
        execute 'h '.expand('<cword>')
      elseif index(keys(g:LanguageClient_serverCommands), &filetype) >= 0
        call LanguageClient#textDocument_hover()
      else
        execute '!' . &keywordprg . " " . expand('<cword>')
      endif
    endfunction

    augroup LanguageClient_config
      autocmd!
      autocmd User LanguageClientStarted call <SID>LcnStarted()
      autocmd User LanguageClientStopped call <SID>LcnStopped()
    augroup END

    Plug 'Shougo/echodoc.vim'
      let g:echodoc#enable_at_startup = 1
      let g:echodoc#type = 'signature'
    Plug 'jackguo380/vim-lsp-cxx-highlight'
    Plug 'm-pilia/vim-ccls'
    return []
endfunction

function s:PlugVimLsp()
  Plug 'prabirshrestha/vim-lsp'
  " LSP client

    Plug 'thomasfaingnaert/vim-lsp-snippets'
    Plug 'thomasfaingnaert/vim-lsp-ultisnips'

    Plug 'jackguo380/vim-lsp-cxx-highlight'
    Plug 'm-pilia/vim-ccls'

    let g:lsp_document_code_action_signs_enabled = 1
    let g:lsp_document_code_action_signs_hint = {'text': 'A'}
    let g:lsp_diagnostics_echo_cursor = 1
    let g:lsp_diagnostics_float_cursor = 1

    let g:vimlsp_servers = [
          \{
          \ 'name': 'rust-analyzer',
          \ 'cmd': {server_info->['rust-analyzer']},
          \ 'allowlist': ['rust'],
          \},
          \{
          \ 'name': 'gopls',
          \ 'cmd': {server_info->['gopls']},
          \ 'allowlist': ['go'],
          \},
          \{
          \ 'name': 'ccls',
          \ 'cmd': {server_info->['ccls']},
          \ 'root_uri':{server_info->lsp#utils#path_to_uri(
          \   lsp#utils#find_nearest_parent_file_directory(
          \     lsp#utils#get_buffer_path(),
          \     ['.ccls', 'compile_commands.json', '.git/']
          \   ))},
          \ 'allowlist': ['c', 'cpp'],
          \ 'initialization_options': {
          \   'highlight': {
          \     'lsRanges': v:true,
          \   }},
          \},
          \{
          \ 'name': 'pyright-langserver',
          \ 'cmd': {server_info->['pyright-langserver', '--lsp']},
          \ 'allowlist': ['python'],
          \},
          \{
          \ 'name': 'ocamllsp',
          \ 'cmd': {server_info->['ocamllsp']},
          \ 'allowlist': ['ocaml'],
          \},
          \{
          \ 'name': 'zls',
          \ 'cmd': {server_info->['zls']},
          \ 'allowlist': ['zig'],
          \},
          \{
          \ 'name': 'haskell-language-server',
          \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
          \ 'allowlist': ['haskell'],
          \ 'initialization_options': {
          \   'haskell': {
          \     'formattingProvider': 'brittany',
          \   }
          \  },
          \},
          \]
    function s:VimLspRegisterServers()
      for l:server_info in g:vimlsp_servers
        call lsp#register_server(l:server_info)
      endfor
    endfunction

    augroup vim_lsp
      autocmd!
      autocmd User lsp_setup call <SID>VimLspRegisterServers()
    augroup END
  return []
endfunction

function s:PlugALE()
  Plug 'dense-analysis/ale'
  " Asynchronous linting

    let g:ale_sign_column_always = 1
    let g:ale_lint_delay = 69

    " Use floating window for hover
    " let g:ale_cursor_detail = 1
    " let g:ale_hover_to_preview = 1
    " let g:ale_floating_preview = 1
    " let g:ale_floating_window_border = ['│', '─', '╭', '╮', '╯', '╰']

    " Use virtualtext for hover
    let g:ale_virtualtext_prefix = '» '
    let g:ale_virtualtext_cursor = 1

    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'

    let g:ale_sign_error = ' ✖'
    let g:ale_sign_warning = ' ‼'
    let g:ale_sign_info = ' ℹ'

    let g:ale_linters = {
          \ 'tex': ['proselint', 'chktex'],
          \ 'rust': ['cargo', 'rustc', 'analyzer'],
          \ 'markdown': ['proselint', 'mdl'],
          \}
    let g:ale_fixers = {
          \ 'rust': ['rustfmt'],
          \ 'bib': ['bibclean'],
          \ 'haskell': ['brittany'],
          \ '*': ['trim_whitespace', 'remove_trailing_lines'],
          \}
    let g:ale_tex_chktex_options = '-n1 -n36 -n26'

    " This way I can do Al<tab> for autocompletion without remembering to capitalize the 'L'
    command! AleInfo ALEInfo
    command! Aleinfo ALEInfo
  return []
endfunction

function s:PlugNeomake()
  Plug 'neomake/neomake'
    let g:neomake_rust_cargo_command = ['check', '--tests']

    function s:NeomakeHooks()
      " These might fail if neomake is not yet installed
      call neomake#configure#automake('nw', 200)
    endfunction
  return [function('s:NeomakeHooks')]
endfunction

function s:PlugNeoformat()
  Plug 'sbdchd/neoformat'
    let g:neoformat_bib_bibclean = {
          \ 'exe': 'bibclean',
          \ 'args': ['-align-equals', '-no-check-values'],
          \ 'stdin': 1,
          \}
    let g:neoformat_tex_latexindent = {
          \ 'exe': 'latexindent',
          \ 'args': ["-y=defaultIndent:\"  \""],
          \ 'stdin': 1,
          \}
    let g:neoformat_enabled_haskell = [ 'stylish-haskell', 'brittany' ]
    nnoremap <leader>aq :Neoformat<CR>
    vnoremap <leader>aq :Neoformat<CR>
  return []
endfunction

function s:PlugSnippets()
  Plug 'honza/vim-snippets'                   " Std lib for snippets
  Plug 'rbonvall/snipmate-snippets-bib'       " Snippets for .bib files
  return []
endfunction

function s:PlugUltisnips()
  Plug 'SirVer/ultisnips'
  " Snippet management
    let g:UltiSnipsExpandTrigger='<M-n>'
    let g:UltiSnipsJumpForwardTrigger='<M-n>'
    let g:UltiSnipsJumpBackwardTrigger='<M-p>'
  return []
endfunction

function s:PlugVista()
  Plug 'liuchengxu/vista.vim'
    let g:vista_fzf_preview = ['right:60%']
    nnoremap <leader>lv :Vista!!<CR>
    nnoremap <leader>lV :Vista show<CR>
    let g:vista_default_executive = 'ctags'
    let g:vista_executive_for = {
        \'vimwiki': 'markdown',
        \'pandoc': 'markdown',
        \'markdown': 'toc',
        \}
  return []
endfunction

function s:PlugCoc()
  let g:completion_tool = 'coc'
  Plug 'neoclide/coc.nvim', { 'branch': 'release' }

    function! s:CocDoc()
      if (index(['vim','help'], &filetype) >= 0)
        execute 'h '.expand('<cword>')
      elseif (coc#rpc#ready())
        call CocActionAsync('doHover')
      else
        execute '!' . &keywordprg . " " . expand('<cword>')
      endif
    endfunction

    " Add Coc to status line
    let g:lightline['component_function']['cocstatus'] = 'coc#status'
    call add(g:lightline['active']['right'], ['cocstatus'])

    " For some reason, Coc is using these weird highlights
    highlight default link FgCocHintFloatBgCocFloating CocHintFloat
    highlight default link FgCocInfoFloatBgCocFloating CocHintFloat
    highlight default link FgCocWarningFloatBgCocFloating CocWarnFloat
    highlight default link FgCocErrorFloatBgCocFloating CocErrorFloat

    augroup coc_autocmds
      autocmd!
      " Allow CoC to update lightline 
      autocmd User CocStatusChange,CocDiagnosticChange call lightline#update()
      " Highlight word under cursor
      autocmd CursorHold * silent call CocActionAsync('highlight')
      " Update signature help on jump placeholder.
      autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
    augroup end

  Plug 'antoinemadec/coc-fzf'

  let g:coc_global_extensions = [
        \'coc-rust-analyzer',
        \'coc-json',
        \'coc-prettier',
        \'coc-go',
        \'coc-lists',
        \'coc-pyright',
        \'coc-sh',
        \'coc-vimlsp',
        \'coc-yaml',
        \'coc-yank',
        \'coc-snippets',
        \'coc-cmake',
        \'coc-clangd',
        \'coc-diagnostic',
        \]

  return []
endfunction

function s:PlugNvimLsp()
  Plug 'neovim/nvim-lspconfig'
  Plug 'nvim-lua/lsp_extensions.nvim'

  " Plug 'glepnir/lspsaga.nvim'
    " Interative UI for nvim-lsp

  Plug 'gfanto/fzf-lsp.nvim'
    " Use FZF as interactive picker
  Plug 'josa42/nvim-lightline-lsp'
    " Integrate with lightline
    " Note that the more popular nvim-lua/lsp-status.nvim seems to provide more
    " features, but there's also more configuration to be done.

  Plug 'folke/trouble.nvim'
    " Summarize diagnostics in document

  Plug 'ray-x/lsp_signature.nvim'
    " Type signature help pop-up

  Plug 'kosayoda/nvim-lightbulb'
    " Make code actions more visible


  function s:SetupNvimLsp()
    " lua require'lspsaga'.init_lsp_saga()
    lua require('fzf_lsp').setup {}
    call add(g:lightline['active']['right'], ['lsp_status'])
    call lightline#lsp#register()
    lua require("trouble").setup {}
    lua require("lsp_signature").setup()
    autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
    sign define LightBulbSign text=↯ texthl=SignColumn
  endfunction

  return [function('s:SetupNvimLsp')]
endfunction

function s:PlugNvimCmp()
  let g:completion_tool = 'cmp'
  Plug 'hrsh7th/nvim-cmp'

  Plug 'onsails/lspkind-nvim'
  " Symbols for lsp kinds

  " Sources
  Plug 'hrsh7th/cmp-buffer'
  Plug 'hrsh7th/cmp-path'
  Plug 'hrsh7th/cmp-nvim-lsp'
  Plug 'hrsh7th/cmp-vsnip'
  Plug 'hrsh7th/cmp-omni'
  Plug 'hrsh7th/cmp-path'
  Plug 'f3fora/cmp-spell'

  function s:SetupNvimCmp()
lua << EOF
  local cmp = require'cmp'
  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body) -- vsnip
      end,
    },
    mapping = {
      ['<C-n>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
      ['<C-p>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
      ['<Down>'] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Select }),
      ['<Up>'] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Select }),
      ['<M-d>'] = cmp.mapping.scroll_docs(-4),
      ['<M-u>'] = cmp.mapping.scroll_docs(4),
      ['<C-x><C-x>'] = cmp.mapping.complete(), -- Force completion pop-up
      ['<C-e>'] = cmp.mapping.close(),
      ['<C-l>'] = cmp.mapping.confirm({
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      })
    },
    sources = {
      { name = 'vsnip' },
      { name = 'nvim_lsp' },
      { name = 'buffer' },
      { name = 'path' },
      { name = 'spell' },
    },
    formatting = {
      format = require'lspkind'.cmp_format({with_text = false, maxwidth = 50})
    },
  })
EOF
  endfunction
  return [function('s:SetupNvimCmp')]
endfunction

function s:PlugNvimVsnip()
  Plug 'hrsh7th/vim-vsnip'
  Plug 'hrsh7th/vim-vsnip-integ'

  imap <expr> <C-l> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
  smap <expr> <C-l> vsnip#available(2) ? '<Plug>(vsnip-expand-or-jump)' : '<C-l>'
  return []
endfunction

function s:PlugNvimCoq()
  Plug 'ms-jpq/coq_nvim', {'branch': 'coq'}
  " To set this up the first time, run :COQdeps

  Plug 'ms-jpq/coq.artifacts', {'branch': 'artifacts'}
  " snippets

  Plug 'ms-jpq/coq.thirdparty', {'branch': '3p'}
  " random collection of extra sources

  let g:coq_settings = {}
  let g:coq_settings['auto_start'] = v:true
  let g:coq_settings['display.pum.fast_close'] = v:false " Avoid flicker
  let g:coq_settings['keymap.bigger_preview'] = '<c-j>'
  let g:coq_settings['keymap.recommended'] = v:false
  let g:coq_settings['keymap.jump_to_mark'] = '<c-l>'
  " let g:coq_settings['keymap.manual_trigger'] = ''

  inoremap <silent><expr> <Esc>   pumvisible() ? "\<C-e><Esc>" : "\<Esc>"
  inoremap <silent><expr> <C-c>   pumvisible() ? "\<C-e><C-c>" : "\<C-c>"
  inoremap <silent><expr> <BS>    pumvisible() ? "\<C-e><BS>"  : "\<BS>"
  " inoremap <silent><expr> <CR>    pumvisible() ? (complete_info().selected == -1 ? "\<C-e><CR>" : "\<C-y>") : "\<CR>"

  return []
endfunction

function s:StackLcn()
  let l:callbacks = []
  let l:callbacks += s:PlugLCN()
  let l:callbacks += s:PlugNcm2()
  " let l:callbacks += s:PlugDeoplete()

  let l:callbacks += s:PlugNeomake()
    for l:ft in keys(g:LanguageClient_serverCommands)
      exec 'let g:neomake_' . l:ft . '_enabled_makers = []'
    endfor

  let l:callbacks += s:PlugNeoformat()

  let l:callbacks += s:PlugSnippets()
  let l:callbacks += s:PlugUltisnips()

  let l:callbacks += s:PlugVista()
    for l:ft in keys(s:LanguageClient_serverCommands)
      let g:vista_executive_for[l:ft] = 'lcn'
    endfor

  let g:which_key_map['l'] = { 'name': '+lsp' }

  function s:LC_keybinds()
    inoremap <C-x>x     <c-r>=ncm2#force_trigger()<cr>
    inoremap <C-x><C-x> <c-r>=ncm2#force_trigger()<cr>

    " inoremap <C-x>x     <c-r>=deoplete#complete()<cr>
    " inoremap <C-x><C-x> <c-r>=deoplete#complete()<cr>

    command! NFmt Neoformat

    if has_key(g:LanguageClient_serverCommands, &filetype)

      nnoremap <silent> <leader>lk :call <SID>LcnDoc()<CR>
      nnoremap <silent> K :call <SID>LcnDoc()<CR>

      nmap <buffer> <silent> <leader>lj <Plug>(lcn-explain-error)

      nmap <buffer> <silent> ]d <Plug>(lcn-diagnostics-next)
      nmap <buffer> <silent> [d <Plug>(lcn-diagnostics-prev)

      nmap <buffer> <silent> <leader>ll <Plug>(lcn-menu)
      nmap <buffer> <silent> <leader>ld <Plug>(lcn-definition)
      nmap <buffer> <silent> <leader>lt <Plug>(lcn-type-definition)
      nmap <buffer> <silent> <leader>li <Plug>(lcn-implementation)

      nmap <buffer> <silent> <leader>l* <Plug>(lcn-references)
      nmap <buffer> <silent> <leader>lr <Plug>(lcn-rename)
      nmap <buffer> <silent> <leader>l/ <Plug>(lcn-symbols)

      nmap <buffer> <silent> <leader>la <Plug>(lcn-code-action)
      nmap <buffer> <silent> <leader>lA <Plug>(lcn-code-lens-action)

      command! LC call LanguageClient_contextMenu()
      command! Fmt call LanguageClient#textDocument_formatting()
    endif
  endfunction

  augroup LanguageClient_keybinds
      autocmd FileType * call <SID>LC_keybinds()
  augroup END

  return l:callbacks
endfunction

function s:StackVimLsp()
  let l:callbacks = []
  let l:callbacks += s:PlugVimLsp()
  let l:callbacks += s:PlugALE()

  Plug 'rhysd/vim-lsp-ale'
  " Integrate ALE With vim-lsp
    let g:ale_disable_lsp = 1
    let g:lsp_ale_diagnostics_severity = 'hint'
    for l:server_info in g:vimlsp_servers
      for l:ft in l:server_info['allowlist']
        if has_key(g:ale_linters, l:ft)
          call add(g:ale_linters[l:ft], 'vim-lsp')
        else
          let g:ale_linters[l:ft] = ['vim-lsp']
        endif
      endfor
    endfor

  let l:callbacks += s:PlugAsyncomplete()
  let l:callbacks += s:PlugUltisnips()
  let l:callbacks += s:PlugSnippets()
  let l:callbacks += s:PlugVista()
    for l:server_info in g:vimlsp_servers
      for l:ft in l:server_info['allowlist']
        let g:vista_executive_for[l:ft] = 'vim_lsp'
      endfor
    endfor

  let g:which_key_map['l'] = { 'name': '+lsp' }
  let g:which_key_map['a'] = { 'name': '+ale' }
    nmap <silent> [a          <Plug>(ale_previous_wrap)
    nmap <silent> ]a          <Plug>(ale_next_wrap)
    nmap <silent> <leader>ax  <Plug>(ale_fix)

  function s:VimLspMappings()
    nmap <silent> <leader>l<space> <Plug>(lsp-hover)
    nmap <silent> <leader>lk <Plug>(lsp-hover)

    nmap <silent> <leader>lr <Plug>(lsp-references)
    nmap <silent> <leader>lR <Plug>(lsp-rename)
    nmap <silent> <leader>la <Plug>(lsp-code-action)
    nmap <silent> <leader>lA <Plug>(lsp-code-lens)

    nmap <silent> <leader>l/ <Plug>(lsp-document-symbol-search)
    nmap <silent> <leader>l? <Plug>(lsp-document-symbol)
    nmap <silent> <leader>ls <Plug>(lsp-workspace-symbol-search)
    nmap <silent> <leader>lS <Plug>(lsp-workspace-symbol)

    nmap <silent> <leader>lq <Plug>(lsp-document-format)

    nmap <silent> <leader>ld <Plug>(lsp-definition)
    nmap <silent> <leader>lD <Plug>(lsp-peek-definition)
    nmap <silent> <leader>li <Plug>(lsp-implementation)
    nmap <silent> <leader>lI <Plug>(lsp-peek-implementation)
    nmap <silent> <leader>lt <Plug>(lsp-type-definition)
    nmap <silent> <leader>lT <Plug>(lsp-peek-type-definition)
    nmap <silent> <leader>lh <Plug>(lsp-signature-help)
    nmap <silent> <leader>lH <Plug>(lsp-type-hierarchy)
    nmap <silent> <leader>le <Plug>(lsp-declaration)
    nmap <silent> <leader>lE <Plug>(lsp-peek-declaration)
    nmap <silent> <leader>lw <Plug>(lsp-preview-focus)
    nmap <silent> <leader>lW <Plug>(lsp-preview-close)

    nmap <silent> <leader>lc <Plug>(lsp-call-hierarchy-incoming)
    nmap <silent> <leader>lC <Plug>(lsp-call-hierarchy-outgoing)
    command! Fmt LspDocumentFormat
  endfunction

  augroup vim_lsp
    autocmd User lsp_setup          call <SID>VimLspMappings()
  augroup END
  return l:callbacks
endfunction

function s:StackCoc()
  let l:callbacks = []
  let l:callbacks += s:PlugVista()
  let l:callbacks += s:PlugCoc()

  " let l:callbacks += s:PlugALE()
  "   let g:ale_disable_lsp = 1
  "
  " nmap <silent> [a          <Plug>(ale_previous_wrap)
  " nmap <silent> ]a          <Plug>(ale_next_wrap)
  " nmap <silent> <leader>ax  <Plug>(ale_fix)
  " nmap <silent> <leader>aj  :ALEDetail<CR>
  "
  "  Also add to :CocConfig
  "diagnostic.displayByAle": true,

  let l:callbacks += s:PlugSnippets()

  inoremap <silent><expr> <c-x><c-x> coc#refresh()

  let g:which_key_map['l'] = { 'name': '+lsp' }

  nmap <silent> [d <Plug>(coc-diagnostic-prev)
  nmap <silent> ]d <Plug>(coc-diagnostic-next)

  nmap <silent> <leader>ld <Plug>(coc-definition)
  nmap <silent> <leader>lD <Plug>(coc-declaration)
  nmap <silent> <leader>lt <Plug>(coc-type-definition)
  nmap <silent> <leader>li <Plug>(coc-implementation)
  nmap <silent> <leader>l= <Plug>(coc-references)

  nnoremap <silent> <leader>lk :call <SID>CocDoc()<CR>
  nnoremap <silent> K :call <SID>CocDoc()<CR>

  nmap <leader>lr <Plug>(coc-rename)
  nmap <leader>lR <Plug>(coc-refactor)

  " Formatting selected code.
  xmap <leader>lq  <Plug>(coc-format-selected)
  nmap <leader>lq  <Plug>(coc-format-selected)

  " Applying codeAction to the selected region.
  xmap <leader>la  <Plug>(coc-codeaction-selected)
  " nmap <leader>la  <Plug>(coc-codeaction-selected)

  " Code action for current cursor position
  nmap <leader>la  <Plug>(coc-codeaction-cursor)

  " Code action for whole file
  nmap <leader>lA  <Plug>(coc-codeaction)

  " Perform codelens action for current line
  nmap <leader>lx <plug>(coc-codelens-action)

  " Apply first quickfix action to problem on the current line
  nmap <leader>lX  <Plug>(coc-fix-current)

  " Do default action for next item.
  nnoremap <silent><nowait> <leader>l]  :<C-u>CocNext<CR>
  " Do default action for previous item.
  nnoremap <silent><nowait> <leader>l[  :<C-u>CocPrev<CR>

  " Show all diagnostics.
  nnoremap <silent><nowait> <leader>lg  :<C-u>CocFzfList diagnostics<cr>
  " Show commands.
  nnoremap <silent><nowait> <leader>ll  :<C-u>CocFzfList commands<cr>
  " Find symbol of current document.
  nnoremap <silent><nowait> <leader>l.  :<C-u>CocFzfList outline<cr>
  " Search workspace symbols.
  nnoremap <silent><nowait> <leader>l/  :<C-u>CocFzfList symbols<cr>
  " Resume latest coc list.
  nnoremap <silent><nowait> <leader>lL  :<C-u>CocFzfListResume<CR>
  " Yank list
  nnoremap <silent> <leader>ly  :<C-u>CocFzfList yank<cr>
  " Close all pop-ups
  nnoremap <silent><nowait> <leader>lz :call coc#float#close_all(1)

  imap <C-s> <Plug>(coc-snippets-expand-jump)

  command! -nargs=0 Fmt  :call CocAction('format')
  command! -nargs=0 Import :call CocAction('runCommand', 'editor.action.organizeImport')
  command! -nargs=? Fold :call CocAction('fold', <f-args>)

  return l:callbacks
endfunction

function s:StackALE()
  let l:callbacks = []
  let l:callbacks += s:PlugALE()

  let l:callbacks += s:PlugDeoplete()
    function s:ALEDeopleteHook()
      call deoplete#custom#option('sources', {
          \ '_': ['ale'],
          \})
    endfunction
  let l:callbacks += [function('s:ALEDeopleteHook')]

  let l:callbacks += s:PlugSnippets()
  let l:callbacks += s:PlugUltisnips()

  let g:which_key_map['a'] = { 'name': '+ale' }

  nmap <silent> [a          <Plug>(ale_previous_wrap)
  nmap <silent> ]a          <Plug>(ale_next_wrap)

  nmap <silent> <leader>ak    <Plug>(ale_hover)
  nmap <silent> <leader>aj    :ALEDetail<CR>

  nmap <silent> <leader>ad    <Plug>(ale_go_to_definition)
  nmap <silent> <leader>asd   <Plug>(ale_go_to_definition_in_split)
  nmap <silent> <leader>avd   <Plug>(ale_go_to_definition_in_vsplit)
  nmap <silent> <leader>at    <Plug>(ale_go_to_type_definition)
  nmap <silent> <leader>ast   <Plug>(ale_go_to_type_definition_in_split)
  nmap <silent> <leader>avt   <Plug>(ale_go_to_type_definition_in_vsplit)

  nmap <silent> <leader>ar    :ALERename<CR>
  nmap <silent> <leader>aa    :ALECodeAction<CR>

  nmap <silent> <leader>a=    <Plug>(ale_find_references)
  nmap <silent> <leader>as=    :ALEFindReferences -split<CR>
  nmap <silent> <leader>av=    :ALEFindReferences -vsplit<CR>

  nmap <silent> <leader>ax    <Plug>(ale_fix)

  command! Fmt ALEFix
  command! Import ALEImport

  return l:callbacks
endfunction

function s:StackNvimLsp()
  let l:callbacks = []

  let g:which_key_map['l'] = { 'name': '+lsp' }

  function s:SetupNvimLspStack()
lua << EOF
  local nvim_lsp = require 'lspconfig'

  local servers = {
    ['clangd'] = nil,
    ['rust_analyzer'] = nil,
    ['pyright'] = nil,
    ['tsserver'] = nil,
    ['hls'] = { haskell = { formattingProvider = 'brittany', } },
    ['vimls'] = nil,
    ['ocamllsp'] = nil,
    ['rnix'] = nil,
  }

  local on_attach = function(_, bufnr)
    vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

    local opts = { noremap = true, silent = true }
    vim.api.nvim_buf_set_keymap(bufnr, 'n', 'K',          '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lk', '<cmd>lua vim.lsp.buf.hover()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>le', '<cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>ld', '<cmd>lua vim.lsp.buf.definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>li', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lh', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>l=', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>la', '<cmd>lua vim.lsp.buf.code_action()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lj', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', opts)

    -- vim.api.nvim_buf_set_keymap(bufnr, 'v', '<leader>ca', '<cmd>lua vim.lsp.buf.range_code_action()<CR>', opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>q', '<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>', opts)
    -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>l/', [[<cmd>lua require('telescope.builtin').lsp_document_symbols()<CR>]], opts)

    vim.api.nvim_buf_set_keymap(bufnr, 'n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)

    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
    vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)

    vim.cmd [[ command! Fmt execute 'lua vim.lsp.buf.formatting()' ]]
  end

  for lsp, settings in pairs(servers) do
    if vim.g.completion_tool == 'cmp' then
      nvim_lsp[lsp].setup {
        on_attach = on_attach,
        capabilities = require'cmp_nvim_lsp'.update_capabilities(vim.lsp.protocol.make_client_capabilities()),
        settings = settings,
      }
    elseif vim.g.completion_tool == 'coq' then
      nvim_lsp[lsp].setup (
        require'coq'.lsp_ensure_capabilities {
          on_attach = on_attach,
          settings = settings,
        }
      )
    else
      nvim_lsp[lsp].setup {
        on_attach = on_attach,
        settings = settings,
      }
    end
  end

  vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
    require('lsp_extensions.workspace.diagnostic').handler, {
      signs = {
        severity_limit = "Error",
      }
    }
  )
  -- vim.lsp.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(
  --  vim.lsp.diagnostic.on_publish_diagnostics, { signs = true, }
  -- )
EOF
    set signcolumn=yes
    sign define LspDiagnosticsSignError text=✖ texthl=LspDiagnosticsSignError
    sign define LspDiagnosticsSignWarning text=‼ texthl=LspDiagnosticsSignWarning
    sign define LspDiagnosticsSignInformation text=ℹ texthl=LspDiagnosticsSignInformation
    sign define LspDiagnosticsSignHint text=» texthl=LspDiagnosticsSignHint

    nnoremap <leader>li :lua require'lsp_extensions'.inlay_hints{ only_current_line = true }
  endfunction

  let l:callbacks += s:PlugNvimLsp()
  " let l:callbacks += s:PlugNvimCmp()
  " let l:callbacks += s:PlugNvimVsnip()
  let l:callbacks += s:PlugNvimCoq()
  let l:callbacks += [function('s:SetupNvimLspStack')]
  return l:callbacks
endfunction

function plugins#ide#setup()
  if !has('nvim')
    return []
  elseif has('nvim-0.5')
    return s:StackNvimLsp()
  else
    return s:StackLcn()
  endif
endfunction

" vim: set ts=2 sw=2 tw=80 et foldlevel=0 :
