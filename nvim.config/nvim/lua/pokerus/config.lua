vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.cmd [[
  augroup highlight_yank
    autocmd!
    autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="CursorLine", timeout=690}
  augroup END
]]

return nil
