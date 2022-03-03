return {
  plug = function(use)
    use {
      "jose-elias-alvarez/null-ls.nvim",
      -- Integrate linters and formatters into LSP
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        local null_ls = require "null-ls"
        null_ls.setup {
          on_attach = require("pokerus.lsp").on_attach,
          sources = {
            null_ls.builtins.formatting.black,
            null_ls.builtins.formatting.prettier.with {
              filetypes = { "html", "json", "yaml", "markdown" },
            },
            null_ls.builtins.diagnostics.shellcheck,
            null_ls.builtins.formatting.shfmt.with {
              extra_args = { "-i", "2", "-ci" },
            },
            null_ls.builtins.diagnostics.vint,
            null_ls.builtins.diagnostics.statix,
            null_ls.builtins.formatting.asmfmt,
            null_ls.builtins.formatting.stylua,
          },
        }
      end,
    }
  end,
}
