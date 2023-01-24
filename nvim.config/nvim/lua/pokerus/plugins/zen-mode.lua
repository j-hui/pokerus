return {
  "folke/zen-mode.nvim",
  cmd = { "ZenMode" },
  opts = {
    twilight = { enabled = true },
  },
  keys = {
    { "<leader>G", function() require("zen-mode").toggle() end, desc = "zen-mode" },
  },
}
