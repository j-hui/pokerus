return {
  "akinsho/toggleterm.nvim",
  version = "*",
  cmd = {
    "ToggleTerm",
    "ToggleTermToggleAll",
    "TermExec",
    "ToggleTermSendCurrentLine",
    "ToggleTermSendVisualLines",
    "ToggleTermSendVisualSelection",
    "ToggleTermSetName",
  },
  keys = {
    { "<C-\\><C-\\>", mode = { "n", "i", "t" }, desc = "toggle-term" },
    { "<C-\\>x", "<cmd>ToggleTerm direction=horizontal<CR>", mode = { "n" }, desc = "toggle-term" },
    { "<C-\\><space>", "<cmd>ToggleTerm direction=float<CR>", desc = "toggle-float-term" },
  },
  config = function()
    require("toggleterm").setup {
      open_mapping = "<C-\\><C-\\>",
      autochdir = true,
    }
  end
}
