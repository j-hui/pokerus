local M = {}

function M.config()
  require("todo-comments").setup {
    highlight = {
      before = "",
      keyword = "fg",
      after = "",
    }
  }
end

function M.plug(use)
  use {
    "folke/todo-comments.nvim",
    config = function()
      require("pokerus.plugins.todo-comments").config()
    end,
  }
end

return M
