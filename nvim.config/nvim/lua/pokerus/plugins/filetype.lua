local M = {}

function M.config()
  require("filetype").setup {}
end

function M.plug(use)
  vim.g.did_load_filetypes = 1
  use {
    "nathom/filetype.nvim",
    config = function()
      require("pokerus.plugins.filetype").config()
    end,
  }
end

return M
