local M = {}

function M.config()
  require("which-key").setup {
    spelling = {
      enabled = true,
    },
    hidden = {
      "<silent>",
      "<cmd>",
      "<Cmd>",
      "<CR>",
      "call",
      "lua",
      "^:",
      "^ ",
      "<Plug>",
      "<plug>",
    },
  }
  require("pokerus").setup_keybinds()
end

function M.plug(use)
  use {
    "folke/which-key.nvim",
    config = function()
      require("pokerus.plugins.which-key").config()
    end,
  }
end

return M
