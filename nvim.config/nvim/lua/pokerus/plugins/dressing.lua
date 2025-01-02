return {
  "stevearc/dressing.nvim",
  -- dependencies = {},
  event = "VeryLazy",
  opts = {
    input = {
      mappings = {
        i = {
          ["<Esc>"] = "Close",
          ["<C-p>"] = "HistoryPrev",
          ["<C-n>"] = "HistoryNext",
        },
      },
    },
    select = {
      backend = { "fzf_lua", "fzf", "builtin", "telescope", "nui" },
    },
  },
  config = true,
}
