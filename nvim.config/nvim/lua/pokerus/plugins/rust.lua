return {
  "simrat39/rust-tools.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },
  ft = "rust",
  config = {
    server = {
      on_attach = require("pokerus.lsp").on_attach,
    },
  },
}
