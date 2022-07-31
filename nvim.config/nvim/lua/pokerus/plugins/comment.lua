local M = {}

function M.config()
  require("Comment").setup {}

  local ft = require('Comment.ft')
  ft({'alex', 'happy'}, {'--%s', '{-%s-}'})

  require("pokerus").nmap {
    g = {
      c = "line-comment",
      b = "block-comment",
    },
  }
end

function M.plug(use)
  use {
    "numToStr/Comment.nvim",
    config = function()
      require("pokerus.plugins.comment").config()
    end,
  }
end

return M
