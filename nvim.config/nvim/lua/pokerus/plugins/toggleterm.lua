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
  },
  config = function()
    require("toggleterm").setup {
      open_mapping = "<C-\\><C-\\>",
      autochdir = true,
      shell = vim.fn.environ().SHELL or vim.o.shell,
      close_on_exit = false,
    }
  end
}
