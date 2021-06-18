scriptencoding utf-8

let s:lsp_servers = {
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

let s:lsp_rootmarkers = {
      \ 'zig': ['build.zig'],
      \ 'vim': ['.git', 'autoload', 'plugin'],
      \}

let g:vista_executive_for = {}

function s:PlugNcm2()
  Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp'
    Plug 'ncm2/ncm2-bufword'                  " Complete from buffer
    Plug 'ncm2/ncm2-path'                     " Complete from path
    Plug 'ncm2/ncm2-markdown-subscope'        " Language subscope for markdown
    Plug 'ncm2/ncm2-ultisnips'                " Snippets
    Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim' " Completion for Vim
    let g:ncm2#popup_delay = 69

    function! s:NcmEnable() abort
      try
        call ncm2#enable_for_buffer()

        inoremap <C-x>x     <c-r>=ncm2#force_trigger()<cr>
        inoremap <C-x><C-x> <c-r>=ncm2#force_trigger()<cr>

        " NOTE: no need to check filetype here since this is handled by 'scope'
        call ncm2#register_source({
                \ 'name': 'vimtex',
                \ 'priority': 8,
                \ 'scope': ['tex'],
                \ 'mark': 'tex',
                \ 'word_pattern': '\w+',
                \ 'complete_pattern': g:vimtex#re#ncm2,
                \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
                \ })
      catch /^Vim\%((\a\+)\)\=:E117/ " Undefined function
        echom 'ncm2 not yet installed'
      catch /^Vim\%((\a\+)\)\=:E121/ " Undefined variable
        echom 'ncm2 source not yet installed'
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

      if exists('g:ale_linters')
        call deoplete#custom#option('sources', {
            \ '_': ['ale'],
            \})
      endif

      inoremap <C-x>x     <c-r>=deoplete#complete()<cr>
      inoremap <C-x><C-x> <c-r>=deoplete#complete()<cr>
      call deoplete#custom#var('omni', 'input_patterns', {
            \ 'tex': g:vimtex#re#deoplete,
            \})
    endfunction
    return [function('s:DeopleteHooks')]
endfunction

function s:PlugLCN()
  Plug 'autozimu/LanguageClient-neovim', {
      \ 'branch': 'next',
      \ 'do': 'bash install.sh',
      \ }

    let g:LanguageClient_loggingLevel = 'INFO'
    let g:LanguageClient_diagnosticsList = 'Quickfix'
    let g:LanguageClient_settingsPath = expand('~/.config/nvim/settings.json')
    let g:LanguageClient_loggingFile = expand('~/.config/nvim/LanguageClient.log')
    let g:LanguageClient_serverCommands = s:lsp_servers
    let g:LanguageClient_rootMarkers = s:lsp_rootmarkers
    for l:ft in keys(s:lsp_servers)
      let g:vista_executive_for[l:ft] = 'lcn'
    endfor

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

    function s:LC_started()
      set signcolumn=yes

      " Taken from https://github.com/autozimu/LanguageClient-neovim/issues/1095
      let g:LanguageClient_fzfOptions =
            \ ['--delimiter', ':', '--preview-window', '+{2}-5'] +
            \ fzf#vim#with_preview().options

      command! LC call LanguageClient_contextMenu()
      command! LCFmt call LanguageClient_textDocument_formatting()
    endfunction

    function s:LC_keybinds()
      if has_key(g:LanguageClient_serverCommands, &filetype)
        nmap <buffer> <silent> <leader>l<space>   <Plug>(lcn-hover)
        nmap <buffer> <silent> <C-h> <Plug>(lcn-explain-error)

        nmap <buffer> <silent> <leader>ll <Plug>(lcn-menu)
        nmap <buffer> <silent> <leader>ld <Plug>(lcn-definition)
        nmap <buffer> <silent> <leader>lt <Plug>(lcn-type-definition)
        nmap <buffer> <silent> <leader>lt <Plug>(lcn-implementation)

        nmap <buffer> <silent> <leader>lr <Plug>(lcn-references)
        nmap <buffer> <silent> <leader>lR <Plug>(lcn-rename)
        nmap <buffer> <silent> <leader>l/ <Plug>(lcn-symbols)

        nmap <buffer> <silent> <leader>la <Plug>(lcn-code-lens-action)
        nmap <buffer> <silent> <leader>lA <Plug>(lcn-code-action)
        nmap <buffer> <silent> <leader>lq <Plug>(lcn-format)

        nmap <buffer> <silent> ]a <Plug>(lcn-diagnostics-next)
        nmap <buffer> <silent> [a <Plug>(lcn-diagnostics-prev)
      endif
    endfunction

    function s:LC_stopped()
      set signcolumn=auto
    endfunction

    augroup LanguageClient_config
      autocmd!
      autocmd User LanguageClientStarted call <SID>LC_started()
      autocmd User LanguageClientStopped call <SID>LC_stopped()
      autocmd FileType * call <SID>LC_keybinds()
    augroup END

    Plug 'Shougo/echodoc.vim'
      let g:echodoc#enable_at_startup = 1
      let g:echodoc#type = 'signature'
    Plug 'jackguo380/vim-lsp-cxx-highlight'
    Plug 'm-pilia/vim-ccls'
    return []
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
    nnoremap <leader>aq :Neoformat<CR>
    vnoremap <leader>aq :Neoformat<CR>
  return []
endfunction

function s:PlugNeomake()
  Plug 'neomake/neomake'
    let g:neomake_rust_cargo_command = ['check', '--tests']
    for l:ft in keys(s:lsp_servers)
      exec 'let g:neomake_' . l:ft . '_enabled_makers = []'
    endfor

    function s:NeomakeHooks()
      " These might fail if neomake is not yet installed
      call neomake#configure#automake('nw', 200)
    endfunction
  return [function('s:NeomakeHooks')]
endfunction

function s:PlugUltisnips()
  Plug 'SirVer/ultisnips'
  " Snippet management
    let g:UltiSnipsExpandTrigger='<M-n>'
    let g:UltiSnipsJumpForwardTrigger='<M-n>'
    let g:UltiSnipsJumpBackwardTrigger='<M-p>'
  Plug 'honza/vim-snippets'                   " Std lib for snippets
  Plug 'rbonvall/snipmate-snippets-bib'       " Snippets for .bib files
  return []
endfunction

function s:PlugALE()
  Plug 'dense-analysis/ale'
  " Asynchronous linting

    let g:ale_sign_column_always = 1
    let g:ale_lint_delay = 100
    let g:ale_virtualtext_cursor = 1

    let g:ale_disable_lsp = 1
    let g:ale_hover_to_preview = 1

    let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
    let g:ale_echo_msg_error_str = 'E'
    let g:ale_echo_msg_warning_str = 'W'
    let g:ale_virtualtext_prefix = '» '
    let g:ale_virtualtext_cursor = 1
    let g:ale_sign_error = ' ✖'
    let g:ale_sign_warning = ' ‼'
    let g:ale_sign_info = ' ℹ'

    let g:ale_linters = {
          \ 'tex': ['proselint', 'chktex'],
          \ 'rust': ['cargo', 'rustc'],
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

    nmap <silent> [a          <Plug>(ale_previous_wrap)
    nmap <silent> ]a          <Plug>(ale_next_wrap)

    nmap <silent> <leader>lx  <Plug>(ale_fix)

  return []
endfunction

function s:PlugVimLsp()
  Plug 'prabirshrestha/vim-lsp'
  " LSP client
  Plug 'rhysd/vim-lsp-ale'
  " Integrate ALE With vim-lsp

  Plug 'thomasfaingnaert/vim-lsp-snippets'
  Plug 'thomasfaingnaert/vim-lsp-ultisnips'

  Plug 'jackguo380/vim-lsp-cxx-highlight'
  Plug 'm-pilia/vim-ccls'

    let g:lsp_document_code_action_signs_enabled = 1
    let g:lsp_document_code_action_signs_hint = {'text': 'A'}
    let g:lsp_diagnostics_echo_cursor = 1
    let g:lsp_diagnostics_float_cursor = 1
    let g:lsp_ale_diagnostics_severity = 'hint'

    for l:ft in keys(s:lsp_servers)
      if has_key(g:ale_linters, l:ft)
        call add(g:ale_linters[l:ft], 'vim-lsp')
      else
        let g:ale_linters[l:ft] = ['vim-lsp']
      endif
    endfor

    function s:VimLspMappings()
      nmap <silent> <leader>l<space> <Plug>(lsp-hover)

      nmap <silent> <leader>lr <Plug>(lsp-references)
      nmap <silent> <leader>lR <Plug>(lsp-rename)
      nmap <silent> <leader>la <Plug>(lsp-code-lens)
      nmap <silent> <leader>lA <Plug>(lsp-code-action)

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

    endfunction

    function s:VimLspRegisterServers()
      for l:ft in keys(s:lsp_servers)
        let g:vista_executive_for[l:ft] = 'vim_lsp'
      endfor

      call lsp#register_server({
            \ 'name': 'rust-analyzer',
            \ 'cmd': {server_info->['rust-analyzer']},
            \ 'allowlist': ['rust'],
            \ })
      call lsp#register_server({
            \ 'name': 'gopls',
            \ 'cmd': {server_info->['gopls']},
            \ 'allowlist': ['go'],
            \ })
      call lsp#register_server({
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
            \ })
      call lsp#register_server({
            \ 'name': 'vim-language-server',
            \ 'cmd': {server_info->['vim-language-server', '--stdio']},
            \ 'allowlist': ['vim'],
            \ })
      call lsp#register_server({
            \ 'name': 'pyright-langserver',
            \ 'cmd': {server_info->['pyright-langserver', '--lsp']},
            \ 'allowlist': ['python'],
            \ })
      call lsp#register_server({
            \ 'name': 'ocamllsp',
            \ 'cmd': {server_info->['ocamllsp']},
            \ 'allowlist': ['ocaml'],
            \ })
      call lsp#register_server({
            \ 'name': 'zls',
            \ 'cmd': {server_info->['zls']},
            \ 'allowlist': ['zig'],
            \ })
      call lsp#register_server({
            \ 'name': 'haskell-language-server',
            \ 'cmd': {server_info->['haskell-language-server-wrapper', '--lsp']},
            \ 'allowlist': ['haskell'],
            \ 'initialization_options': {
            \   'haskell': {
            \     'formattingProvider': 'brittany',
            \   }},
            \ })
    endfunction

    augroup vim_lsp
      autocmd!
      autocmd User lsp_setup          call <SID>VimLspRegisterServers()
      autocmd User lsp_setup          call <SID>VimLspMappings()
    augroup END
  return []
endfunction

function s:PlugAsyncomplete()
  Plug 'prabirshrestha/asyncomplete.vim'
    Plug 'prabirshrestha/asyncomplete-buffer.vim'
    Plug 'prabirshrestha/asyncomplete-file.vim'
    Plug 'prabirshrestha/asyncomplete-lsp.vim'
    Plug 'andreypopp/asyncomplete-ale.vim'
    Plug 'prabirshrestha/asyncomplete-ultisnips.vim'


    function s:AsyncompleteHooks()
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
    endfunction

  return [function('s:AsyncompleteHooks')]
endfunction

function s:PlugVista()
  Plug 'liuchengxu/vista.vim'
    let g:vista_fzf_preview = ['right:60%']
    nnoremap <leader>lv :Vista!!<CR>
    nnoremap <leader>lV :Vista show<CR>

  return []
endfunction

function plugins#ide#setup()
  if !has('nvim')
    return []
  endif
  let g:which_key_map['l'] = { 'name': '+lsp' }

  let l:callbacks = []
  " let l:callbacks += s:PlugLCN()
  " let l:callbacks += s:PlugNcm2()
  " let l:callbacks += s:PlugNeomake()
  " let l:callbacks += s:PlugNeoformat()

  let l:callbacks += s:PlugALE()
  let l:callbacks += s:PlugVimLsp()
  let l:callbacks += s:PlugAsyncomplete()

  " let l:callbacks += s:PlugDeoplete()
  let l:callbacks += s:PlugUltisnips()
  let l:callbacks += s:PlugVista()
  return l:callbacks
endfunction

" vim: set ts=2 sw=2 tw=80 et foldlevel=0 :
