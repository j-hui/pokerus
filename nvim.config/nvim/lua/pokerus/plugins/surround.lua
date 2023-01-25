return {
  "kylechui/nvim-surround",
  version = "*",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-treesitter/nvim-treesitter-textobjects",
  },
  opts = {
    aliases = {
      ["q"] = { '"', "'", "`" },
      ["s"] = { "}", "]", ")", ">", '"', "'", "`" },
    },
    highlight = {
      duration = 0,
   },
   move_cursor = false,
  },
  event = "VeryLazy",
}
