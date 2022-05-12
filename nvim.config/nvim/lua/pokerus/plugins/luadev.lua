local M = {}

function M.config()
  -- TODO: open luadev buffer automatically
  -- TODO: binding to close luadev buffer
  -- TODO: evaluate text object based on treesitter nodes

  require("pokerus").nmap ({
    name = "+luadev",
    ["<space>"] = {
      "<cmd>Luadev<CR>"
      , "lua-buffer",
    },
    ["c"] = {
      "<Plug>(Luadev-Run)",
      "lua-run",
    },
    ["x"] = {
      "<Plug>(Luadev-RunLine)",
      "lua-run-line",
    },
    ["k"] = {
      "<Plug>(Luadev-RunWord)",
      "lua-run-line",
    },
  }, {prefix = "<leader>x"})
end

function M.plug(use)
  use {
    "bfredl/nvim-luadev",
    config = function()
      require("pokerus.plugins.luadev").config()
    end,
  }
end

return M
