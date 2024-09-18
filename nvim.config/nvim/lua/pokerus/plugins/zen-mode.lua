return {
  {
    "folke/zen-mode.nvim",
    dependencies = { "folke/twilight.nvim" },
    cmd = { "ZenMode" },
    opts = {
      twilight = { enabled = true },
    },
    keys = {
      { "<leader>T", mode = { "n" }, "<cmd>ZenMode<cr>", desc = "zen-mode" }
    },
  },
  {
    "folke/twilight.nvim",
    cmd = { "Twilight", "TwilightEnable", "TwilightDisable" },
    keys = {
      { "<leader>t", mode = { "n" }, "<cmd>Twilight<cr>", desc = "twilight" }
    },
    opts = {
      expand = { -- for treesitter, we we always try to expand to the top-most ancestor with these types
        "function",
        "method",
        "table",
        "if_statement",
        "section",
      },
      exclude = {}, -- exclude these filetypes
    }
  }
}
