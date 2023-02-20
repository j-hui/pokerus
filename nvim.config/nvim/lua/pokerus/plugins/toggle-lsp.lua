return {
  "adoyle-h/lsp-toggle.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
  },
  opts = {
    create_cmds = true,
    telescope = true,
  },
  cmd = { "ToggleLSP", "ToggleNullLSP" },
  keys = {
    { "<leader>lq", "<cmd>ToggleLSP<CR>", mode = "n", desc = "lsp-toggle" },
  },
}
