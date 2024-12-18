return {
  "saecki/crates.nvim",
  event = { "BufRead Cargo.toml" },
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  opts = {
    lsp = {
      enabled = true,
      on_attach = function(client, bufnr)
        require("pokerus.lsp").on_attach(client, bufnr)
      end,
      actions = true,
      completion = true,
      hover = true,
    },
  },
}
