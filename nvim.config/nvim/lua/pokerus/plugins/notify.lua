local M = {}

function M.config()
  vim.notify = require "notify"

  require("pokerus").nmap {
    ["<leader>q"] = {
      function()
        vim.notify.dismiss()
      end,
      "notifications-clear",
    },
  }
end

function M.plug(use)
  use {
    "rcarriga/nvim-notify",
    config = function()
      require("pokerus.plugins.notify").config()
    end,
  }
end

return M
