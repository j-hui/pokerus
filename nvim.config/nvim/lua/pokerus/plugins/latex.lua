return {
  "KeitaNakamura/tex-conceal.vim",
  ft = "tex",
  init = function()
    vim.api.nvim_create_autocmd("Filetype", {
      pattern = "tex",
      callback = function()
        vim.b.endwise_addition = [[\="\\end" . matchstr(submatch(0), "{.\\{-}}")]]
        vim.b.endwise_words = [[begin]]
        vim.b.endwise_pattern = [[\\begin{.\{-}}]]
        vim.b.endwise_syngroups = [[texSection,texBeginEnd,texBeginEndName,texStatement]]

        vim.keymap.set("i", "<C-g>/", "\\emph{}<left>", { desc = "latex-emph" })
        vim.keymap.set("i", "<C-g>t", "\\texttt{}<left>", { desc = "latex-texttt" })
        vim.keymap.set("i", "<C-g>b", "\\textbf{}<left>", { desc = "latex-textbf" })
        vim.keymap.set("i", "<C-g>i", "\\textit{}<left>", { desc = "latex-textit" })
      end
    })
  end,
}

--[[ NOTE: recover my old vimtex config (if I ever use it again)
function s:StackVimtex()
  Plug 'lervag/vimtex'
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
]]
