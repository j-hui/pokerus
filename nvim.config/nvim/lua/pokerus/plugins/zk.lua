local M = {}

function M.config()
  require("zk").setup {}
end

function M.plug(use)
  use {
    "mickael-menu/zk-nvim",
    opt = true,
    event = "VimEnter",
    config = function()
      require("pokerus.plugins.zk").config()
    end,
  }
end

return M
