local M = {}

function M.filetype(ft, cb)
  vim.api.nvim_create_autocmd("FileType", { pattern = ft, callback = cb })
end

function M.colorscheme(cb)
  vim.api.nvim_create_autocmd("ColorScheme", { callback = cb })
end

return M
