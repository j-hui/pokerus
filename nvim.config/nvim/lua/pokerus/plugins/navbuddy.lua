return {
  "SmiteshP/nvim-navbuddy",
  dependencies = {
    "neovim/nvim-lspconfig",
    "SmiteshP/nvim-navic",
    "MunifTanjim/nui.nvim"
  },
  cmd = { "Navbuddy" },
  keys = { { "<leader>v", "<Cmd>Navbuddy<CR>", desc = "navbuddy" }, },
  opts = {
    lsp = {
      auto_attach = true,
    },
  },
  lazy = false, -- Need to set this up early so it can attach to LSP clients
}
