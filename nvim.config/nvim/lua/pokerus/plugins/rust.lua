return {
  "simrat39/rust-tools.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },
  ft = "rust",
  config = {
    server = {
      settings = {
        ["rust-analyzer"] = {
          -- cargo = { features = { "mlua" } },
          check = {
            command = "clippy",
          },
          diagnostics = {
            disabled = { "unresolved-proc-macro" },
          },
        },
      },
      on_attach = require("pokerus.lsp").on_attach,
    },
  },
}
