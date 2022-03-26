local M = {}

function M.plug(use)
  use {
    "rcarriga/nvim-notify",
    config = function()
      vim.notify = require "notify"
    end,
  }
end

return M
