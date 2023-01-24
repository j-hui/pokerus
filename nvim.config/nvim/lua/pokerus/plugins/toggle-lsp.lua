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
    { "<leader>l<BS>", "<cmd>ToggleLSP<CR>", mode = "n", desc = "lsp-toggle" },
  },
}
