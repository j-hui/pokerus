local M = {}

function M.config()
  local npairs = require "nvim-autopairs"

  npairs.setup {
    disable_in_macro = true,
    disable_in_visualblock = true,
    map_c_w = true,
    fast_wrap = {
      map = "<C-j>",
    },
    check_ts = true,
    ts_config = {
      lua = { "string" }, -- it will not add pair on that treesitter node
      javascript = { "template_string" },
    },
  }

  local R = require "nvim-autopairs.rule"
  -- Adapted from: https://github.com/IndianBoy42/LunarVim/blob/0ec62e6ef5dff125065557347f840a65bea580a1/lua/lv-autopairs/init.lua

  npairs.add_rules {
    R("|", "|", "rust"),
  }

  npairs.add_rules {
    R("`", "'", "tex"),
  }

  local texmods = {
    ["\\left"] = "\\right",
    ["\\big"] = "\\big",
    ["\\bigg"] = "\\bigg",
    ["\\Big"] = "\\Big",
    ["\\Bigg"] = "\\Bigg",
  }
  local texpairs = {
    ["\\("] = "\\)",
    ["\\["] = "\\]",
    ["\\{"] = "\\}",
    ["\\|"] = "\\|",
    ["\\langle "] = "\\rangle",
    ["\\lceil "] = "\\rceil",
    ["\\lfloor "] = "\\rfloor",
  }
  local basicpairs = {
    ["("] = ")",
    ["["] = "]",
    ["{"] = "}",
  }

  for lm, rm in pairs(texmods) do
    for lp, rp in pairs(texpairs) do
      npairs.add_rule(R(lm .. lp, " " .. rm .. rp, "tex"))
    end
    for lp, rp in pairs(basicpairs) do
      npairs.add_rule(R(lm .. lp, " " .. rm .. rp, "tex"))
    end
  end
  for lp, rp in pairs(texpairs) do
    npairs.add_rule(R(lp, rp, "tex"))
  end
end

function M.plug(use)
  use {
    "windwp/nvim-autopairs",
    opt = true,
    event = "VimEnter",
    requires = {
      "nvim-treesitter/nvim-treesitter",
    },
    config = function()
      require("pokerus.plugins.autopairs").config()
    end,
  }
end

return M