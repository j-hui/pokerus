local M = {}

function M.ft_callback(ft, cb)
  vim.api.nvim_create_autocmd("FileType", { pattern = ft, callback = cb })
end

return M
