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

function s:PlugNcm2()
  Plug 'ncm2/ncm2' | Plug 'roxma/nvim-yarp'
    Plug 'ncm2/ncm2-bufword'                  " Complete from buffer
    Plug 'ncm2/ncm2-path'                     " Complete from path
    Plug 'ncm2/ncm2-markdown-subscope'        " Language subscope for markdown
    Plug 'ncm2/ncm2-ultisnips'                " Snippets
    Plug 'ncm2/ncm2-vim' | Plug 'Shougo/neco-vim' " Completion for Vim
    let g:ncm2#popup_delay = 169

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
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }

    Plug 'deoplete-plugins/deoplete-dictionary'
    " Auto-complete dictionary word

    let g:deoplete#enable_at_startup = 1

    function s:DeopleteHooks()
      " These might fail if deoplete is not yet installed
      call deoplete#custom#option('auto_complete_delay', 200)
      call deoplete#custom#option('smart_case', v:true)
      call deoplete#custom#option('auto_refresh_delay', 500)
      set completeopt-=preview

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
        nnoremap <buffer> gK K
        nmap <buffer> <silent> K <Plug>(lcn-hover)
        nmap <buffer> <silent> <C-]> <Plug>(lcn-definition)
        nmap <buffer> <silent> <C-h> <Plug>(lcn-explain-error)
        nmap <buffer> <silent> g/ <Plug>(lcn-references)
        nmap <buffer> <silent> g? <Plug>(lcn-symbols)
        nmap <buffer> <silent> gr <Plug>(lcn-rename)
        nmap <buffer> <silent> gx <Plug>(lcn-code-lens-action)
        nmap <buffer> <silent> gX <Plug>(lcn-code-action)
        nmap <buffer> <silent> gQ <Plug>(lcn-format)
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
    nnoremap gl :Neoformat<CR>
    vnoremap gl :Neoformat<CR>
  return []
endfunction

function s:PlugNeomake()
  Plug 'neomake/neomake'
    let g:neomake_rust_cargo_command = ['check', '--tests']
    for ft in keys(s:lsp_servers)
      exec 'let g:neomake_' . ft . '_enabled_makers = []'
    endfor

    function s:NeomakeHooks()
      " These might fail if neomake is not yet installed
      call neomake#configure#automake('nw', 200)
    endfunction
  return [function('s:NeomakeHooks')]
endfunction

function s:PlugUltisnips()
  Plug 'SirVer/ultisnips'                     " Snippet management
    let g:UltiSnipsExpandTrigger='<M-n>'
    let g:UltiSnipsJumpForwardTrigger='<M-n>'
    let g:UltiSnipsJumpBackwardTrigger='<M-p>'
  Plug 'honza/vim-snippets'                   " Std lib for snippets
  Plug 'rbonvall/snipmate-snippets-bib'       " Snippets for .bib files
  return []
endfunction

function plugins#ide#setup()
  let l:callbacks = []
  let l:callbacks += s:PlugLCN()
  let l:callbacks += s:PlugNcm2()
  let l:callbacks += s:PlugNeomake()
  let l:callbacks += s:PlugNeoformat()
  let l:callbacks += s:PlugUltisnips()
  return l:callbacks
endfunction

" vim: set ts=2 sw=2 tw=80 et foldlevel=0 :
