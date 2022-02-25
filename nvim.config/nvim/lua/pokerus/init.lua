local M = {}

M.map = function(mappings, opt, override_opt)
  -- Thin wrapper around folke/which-key.nvim
  require("which-key").register(
    mappings,
    vim.tbl_extend("force", opt, override_opt)
  )
end

M.lmap = function(mappings, opt)
  M.map(mappings, opt, { prefix = "<leader>" })
end

M.nmap = function(mappings, opt)
  M.map(mappings, opt, { mode = "n" })
end

M.imap = function(mappings, opt)
  M.map(mappings, opt, { mode = "i" })
end

M.vmap = function(mappings, opt)
  M.map(mappings, opt, { mode = "v" })
end

M.omap = function(mappings, opt)
  M.map(mappings, opt, { mode = "o" })
end

M.xmap = function(mappings, opt)
  M.map(mappings, opt, { mode = "x" })
end

return M
