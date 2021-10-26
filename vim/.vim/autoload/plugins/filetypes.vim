function s:PlugCoq()
  Plug 'whonore/coqtail', { 'for': 'coq' }
    augroup CoqtailHighlights
      autocmd!
      autocmd ColorScheme *
            \   highlight def CoqtailChecked ctermbg=236
            \|  highlight def CoqtailSent    ctermbg=237
    augroup END
    let g:coqtail_match_shift = 1
    let g:coqtail_indent_on_dot = 1
    let g:coqtail_auto_set_proof_diffs = 'on'

    let g:coqtail_update_tagstack = 1

    " let g:coqtail_map_prefix = '<C-c>'
    let g:coqtail_nomap = 1

    function DefineCoqtailMappings()
        call g:WhichKeyL(['c', 'name'], '+coqtail')

        " Coqtail control
        nmap <buffer> <leader>cQ        <Plug>CoqStart
        nmap <buffer> <leader>cq        <Plug>CoqStop
        nmap <buffer> <leader>cD        <Plug>CoqToggleDebug
        nmap <buffer> <leader>cr        <Plug>CoqRestorePanels

        " Checked region navigation
        nmap <buffer> <leader>cl             <Plug>CoqToLine
        nmap <buffer> <leader>cc             mz$<Plug>CoqToLine`z
        imap <buffer> <C-c>l            <Esc><Plug>CoqToLine
        imap <buffer> <C-c>cc           <Esc>mz$<Plug>CoqToLine`z

        nmap <buffer> <leader>cj        <Plug>CoqNext
        nmap <buffer> <leader>ck        <Plug>CoqUndo
        nmap <buffer> <leader>ch        <Plug>CoqJumpToEnd

        " Goal buffer navigation
        nmap <buffer> <leader>cg        <Plug>CoqGotoGoalStart
        nmap <buffer> <leader>cG        <Plug>CoqGotoGoalEnd
        nmap <buffer> ]c                <Plug>CoqGotoGoalNextStart
        nmap <buffer> ]C                <Plug>CoqGotoGoalNextEnd
        nmap <buffer> [c                <Plug>CoqGotoGoalPrevStart
        nmap <buffer> [C                <Plug>CoqGotoGoalPrevEnd

        " Semantic features
        nmap <buffer> <c-]>             <Plug>CoqGotoDef
        nmap <buffer> <leader>cc        <Plug>CoqCheck
        xmap <buffer> <leader>cc        <Plug>CoqCheck
        nmap <buffer> <leader>ca        <Plug>CoqAbout
        xmap <buffer> <leader>ca        <Plug>CoqAbout
        nmap <buffer> <leader>cs        <Plug>CoqSearch
        xmap <buffer> <leader>cs        <Plug>CoqSearch
        nmap <buffer> <leader>cd        <Plug>CoqPrint
        xmap <buffer> <leader>cd        <Plug>CoqPrint
        nmap <buffer> <leader>cf        <Plug>CoqLocate
        xmap <buffer> <leader>cf        <Plug>CoqLocate
    endfunction

    augroup coqtail_mappings
      autocmd!
      autocmd Filetype coq,coq-infos,coq-goals call DefineCoqtailMappings()
      autocmd Filetype coq syntax sync fromstart
    augroup END
endfunction

function s:PlugLatex()
  Plug 'lervag/vimtex',   { 'for': 'tex' }
    let g:tex_flavor = 'latex'
    let g:vimtex_compiler_latexmk = {
      \ 'continuous' : 0,
      \}

    " Automatically open quickfix, but do not focus
    let g:vimtex_quickfix_mode = 2
    let g:vimtex_quickfix_autoclose_after_keystrokes = 4
    let g:vimtex_quickfix_ignore_filters = [
      \ 'Font shape declaration has incorrect series value',
      \ 'You are using breakurl while processing',
      \ 'Underfull',
      \ 'Overfull',
      \]

    let g:tex_conceal = 'abdmg'
    let g:vimtex_mappings_enabled = 0
    let g:vimtex_imaps_enabled = 0
    let g:vimtex_view_method = 'zathura'
    let g:vimtex_complete_enabled = 1
    " let g:vimtex_disable_recursive_main_file_detection = 1

    if exists('g:coc_global_extensions')
      let g:coc_global_extensions += ['coc-vimtex']
    endif

    function! VimtexConfig()
      call g:WhichKeyL(['c', 'name'], '+vimtex')
      nmap <buffer> <leader>cc        <plug>(vimtex-compile-ss)
      nmap <buffer> <leader>c<Space>  <plug>(vimtex-view)
      nmap <buffer> <leader>ce        <plug>(vimtex-errors)

      imap <buffer> <C-g>]            <plug>(vimtex-delim-close)
      imap <buffer> <C-g>/            \emph{}<left>
      imap <buffer> <C-g>t            \texttt{}<left>
      imap <buffer> <C-g>b            \textbf{}<left>
      imap <buffer> <C-g>i            \textit{}<left>

      try
        if g:completion_tool ==# 'ncm2'
          call ncm2#register_source({
              \ 'name': 'vimtex',
              \ 'priority': 8,
              \ 'scope': ['tex'],
              \ 'mark': 'tex',
              \ 'word_pattern': '\w+',
              \ 'complete_pattern': g:vimtex#re#ncm2,
              \ 'on_complete': ['ncm2#on_complete#omni', 'vimtex#complete#omnifunc'],
              \ })
        elseif g:completion_tool ==# 'deoplete'
          call deoplete#custom#var('omni', 'input_patterns', {
                \ 'tex': g:vimtex#re#deoplete,
                \})
        elseif g:completion_tool ==# 'coc'
          nmap <buffer> K <Plug>(vimtex-doc-package)
        endif
      catch /^Vim\%((\a\+)\)\=:E117/ " Undefined function
        echom 'Completion tool ' . g:completion_tool . '/vimtex not yet installed'
      catch /^Vim\%((\a\+)\)\=:E121/ " Undefined variable
        echom 'Completion tool ' . g:completion_tool . '/vimtex not yet installed'
      endtry
    endfunction

    augroup vimtex_settings
      autocmd!
      autocmd Filetype tex call VimtexConfig()
    augroup END
endfunction

function s:PlugMarkdown()
  Plug 'tpope/vim-markdown',                { 'as': 'tpope-vim-markdown' }
    let g:markdown_fenced_languages = [
          \ 'html',
          \ 'python',
          \ 'bash=sh',
          \ 'c',
          \ 'cpp',
          \ 'ocaml',
          \ 'haskell',
          \ 'rust',
          \ ]
    let g:markdown_folding = 1
    let g:markdown_syntax_conceal = 1

  " Plug 'plasticboy/vim-markdown', { 'as': 'plasticboy-vim-markdown' }
    let g:vim_markdown_fenced_languages = g:markdown_fenced_languages
  "   let g:vim_markdown_auto_insert_bullets = 0
  "   let g:vim_markdown_folding_style_pythonic = 1
  "   " let g:vim_markdown_math = 1
  "   function MarkdownHook()
  "     nmap g] <Plug>Markdown_MoveToCurHeader
  "     nmap g[ <Plug>Markdown_MoveToParentHeader
  "   endfunction

  "   augroup markdown_mappings
  "     autocmd!
  "     autocmd Filetype markdown call MarkdownHook()
  "   augroup END

  " Plug 'gabrielelana/vim-markdown',         { 'as': 'gabrielelana-vim-markdown'}
    " let g:markdown_enable_mappings = 0
    " let g:markdown_enable_folding = 1
    " let g:markdown_enable_input_abbreviations = 0
  if has('nvim-0.5')
    Plug 'ellisonleao/glow.nvim'
  endif
endfunction

function s:PlugHaskell()
  Plug 'neovimhaskell/haskell-vim'
    let g:haskell_enable_quantification = 1   " to enable highlighting of `forall`
    let g:haskell_enable_recursivedo = 1      " to enable highlighting of `mdo` and `rec`
    let g:haskell_enable_arrowsyntax = 1      " to enable highlighting of `proc`
    let g:haskell_enable_pattern_synonyms = 1 " to enable highlighting of `pattern`
    let g:haskell_enable_typeroles = 1        " to enable highlighting of type roles
    let g:haskell_enable_static_pointers = 1  " to enable highlighting of `static`
    let g:haskell_backpack = 1                " to enable highlighting of backpack keywords
  Plug 'andy-morris/happy.vim'
  Plug 'andy-morris/alex.vim'

endfunction

function s:PlugCCpp()
  Plug 'octol/vim-cpp-enhanced-highlight'
  " Better highlighting for C/C++
    let g:cpp_posix_standard = 1
    let g:cpp_class_scope_highlight = 1
    let g:cpp_member_variable_highlight = 1
    let g:cpp_class_decl_highlight = 1
    let g:cpp_concepts_highlight = 1
endfunction

function s:PlugGo()
  Plug 'fatih/vim-go'
    " Disable features to let LSP configuration do the work
    let g:go_echo_go_info = 0
    let g:go_code_completion_enabled = 0
    let g:go_fmt_autosave = 0
    let g:go_gopls_enabled = 0
    let g:go_def_mapping_enabled = 0
endfunction

function s:PlugValgrind()
  Plug 'j-hui/valgrind.vim'
    let g:valgrind_arguments='--leak-check=yes --num-callers=64'
    let g:valgrind_strip_program_output = 0

    function ValgrindHook()
      nmap [v <Plug>ValgrindStackUp
      nmap ]v <Plug>ValgrindStackDown
    endfunction

    augroup valgrind_hook
      autocmd!
      autocmd User ValgrindEnter call ValgrindHook()
    augroup END
endfunction

function s:PlugZig()
  Plug 'ziglang/zig.vim'
    let g:zig_fmt_autosave = 0
endfunction

function s:PlugWiki()
  Plug 'lervag/wiki.vim'
  Plug 'lervag/wiki-ft.vim'
    let g:wiki_root = '~/wiki'

    let g:wiki_filetypes = ['wiki']
    let g:wiki_link_extension = '.wiki'
    call g:WhichKeyL(['w', 'name'], '+wiki')
    let g:wiki_link_target_type = 'wiki'
    let g:wiki_write_on_nav = 1

    let g:wiki_mappings_use_defaults = 'none'

    nmap <leader>wi <plug>(wiki-index)
    nmap <leader>wj <plug>(wiki-journal)
    nmap <leader>wo <plug>(wiki-open)
    nmap <leader>we <plug>(wiki-fzf-pages)
    call g:WhichKeyL(['w', 'e'], 'fzf-wiki-open')
    " Additional wiki ft-only mappings in after/ftplugin/wiki.vim
    let g:wiki_fzf_pages_force_create_key = 'ctrl-j'

    let g:wiki_map_link_create = 'WikiLinkSpaceToHyphen'
    let g:wiki_map_create_page = 'WikiLinkSpaceToHyphen'

    function WikiLinkSpaceToHyphen(text) abort
      let l:lowered = tolower(a:text)
      let l:no_spaces = substitute(l:lowered, '\s\+', '-', 'g')
      let l:no_quotes = substitute(substitute(l:no_spaces, '"', '', 'g'), "'", '', 'g')
      let l:no_symbols = substitute(l:no_quotes, '[!?\\\$~]', '', 'g')
      return l:no_symbols
    endfunction

    function! s:VimWikiConfig()
      if g:completion_tool ==# 'ncm2'
        call ncm2#register_source({
            \ 'name': 'wiki',
            \ 'priority': 9,
            \ 'scope': ['wiki'],
            \ 'word_pattern': '\w+',
            \ 'complete_pattern': '\[\[',
            \ 'on_complete': ['ncm2#on_complete#delay', 200,
            \                 'ncm2#on_complete#omni', 'wiki#complete#omnicomplete'],
            \ })
      endif
    endfunction

    augroup vimwiki_settings
      autocmd!
      autocmd User WikiBufferInitialized call s:VimWikiConfig()
    augroup END
endfunction

function s:PlugMail()
  " Plug 'felipec/notmuch-vim'
  Plug 'adborden/vim-notmuch-address', { 'for': 'mail' }
endfunction

function s:PlugMisc()
  Plug 'leanprover/lean.vim'
  Plug 'idris-hackers/idris-vim'
  Plug 'LnL7/vim-nix'
  Plug 'blyoa/vim-promela-syntax'
  Plug 'chrisbra/csv.vim'
  Plug 'rust-lang/rust.vim'
    let g:rust_fold = 1
  Plug 'leafgarland/typescript-vim'
  Plug 'keith/swift.vim'
  Plug 'dag/vim-fish'
  Plug 'cespare/vim-toml'
  Plug 'neomutt/neomutt.vim'
  Plug 'liuchengxu/graphviz.vim'
  Plug 'editorconfig/editorconfig-vim'
  Plug 'mboughaba/i3config.vim'
endfunction

function plugins#filetypes#setup()
  call s:PlugCoq()
  call s:PlugLatex()
  call s:PlugMarkdown()
  call s:PlugHaskell()
  call s:PlugCCpp()
  call s:PlugGo()
  call s:PlugValgrind()
  call s:PlugZig()
  call s:PlugMisc()
  call s:PlugMail()
  call s:PlugWiki()
  return []
endfunction

" vim: set ts=2 sw=2 tw=80 et :
