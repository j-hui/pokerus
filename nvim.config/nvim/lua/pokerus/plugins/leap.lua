return {
  "ggandor/leap.nvim",
  dependencies = { "ggandor/flit.nvim", "ggandor/leap-spooky.nvim", "ggandor/leap-ast.nvim" },
  keys = {
    -- flit
    { "f", mode = { "n", "x", "o" } },
    { "F", mode = { "n", "x", "o" } },
    { "t", mode = { "n", "x", "o" } },
    { "T", mode = { "n", "x", "o" } },

    -- leap-ast
    { "gt", function() require("leap-ast").leap() end, mode = { "n", "x", "o" }, desc = "leap-ast" },
  },
  config = function()
    require('flit').setup()
  end,
}
