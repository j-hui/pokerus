local M = {}

function M.config()
  require("pokerus").vimsetup("markdown")
end

function M.plug(use)
  use {
    "preservim/vim-markdown",
    config = function()
      require("pokerus.plugins.markdown").config()
    end,
  }
  use "ellisonleao/glow.nvim"
end

return M
