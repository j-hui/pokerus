local M = {}

function M.config()
  require("git-conflict").setup {
    default_mappings = false,
    disable_diagnostics = true,
  }
end

function M.plug(use)
  use {
    "akinsho/git-conflict.nvim",
    config = function()
      require("pokerus.plugins.git-conflict").config()
    end,
  }
end

return M
