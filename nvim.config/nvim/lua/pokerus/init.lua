local M = {}

M.map = function(mappings, opt, override_opt)
  opt = opt or {}
  override_opt = override_opt or {}
  -- Thin wrapper around folke/which-key.nvim
  require("which-key").register(
    mappings,
    vim.tbl_extend("force", opt, override_opt)
  )
end

M.nmap = function(mappings, opt)
  -- M.map(mappings, opt, { mode = "n" })
  M.map(mappings, opt, {})
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

M.setup_vim_maps = function()
  M.map {
    g = {
      ["-"] = "open-parent-dir",
      ["="]= "open-cwd",
    }
  }
end

M.vimsetup = function(name)
  vim.fn["pokerus#plugins#" .. name .. "#setup"]()
end


return M
