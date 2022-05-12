local M = {}

function M.config()
  require("fidget").setup {
    debug = {
      logging = true,
    },
  }
end

function M.plug(use)
  use {
    "j-hui/fidget.nvim",
    requires = { "neovim/nvim-lspconfig" },
    config = function()
      require("pokerus.plugins.fidget").config()
    end,
  }
end

return M
