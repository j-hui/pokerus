return {
  setup = function()
    vim.cmd [[
      augroup highlight_yank
        autocmd!
        autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="CursorLine", timeout=690}
      augroup END
    ]]
  end
}
