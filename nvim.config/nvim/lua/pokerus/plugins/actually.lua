local M = {}

function M.config()
end

function M.plug(use)
  use {
    'mong8se/actually.nvim',
    config = function()
      require("pokerus.plugins.actually").config()
    end,
  }
end

return M
