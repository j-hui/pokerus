-- TODO: annotate prefix names
return {
  "folke/which-key.nvim",
  opts = {
    spelling = {
      enabled = true,
    },
    hidden = {
      "<silent>",
      "<cmd>",
      "<Cmd>",
      "<CR>",
      "call",
      "lua",
      "^:",
      "^ ",
      "<Plug>",
      "<plug>",
    },
  }
}
