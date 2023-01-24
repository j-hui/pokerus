return {
  "jose-elias-alvarez/null-ls.nvim",
  init = function()
    vim.api.nvim_create_autocmd('LspAttach', {
      callback = function(args)
        -- When I use gq, I don't want LSP to perform formatting for me for most kinds of buffers.
        -- See: https://github.com/jose-elias-alvarez/null-ls.nvim/issues/1130#issuecomment-1268760653
        vim.bo[args.buf].formatexpr = nil
      end,
    })
  end,
  ft = {
    "python",
    "html",
    "json",
    "yaml",
    "markdown",
    "sh",
    "bash",
    "zsh",
    "vim",
    "nix",
    "asmfmt",
  },
  config = function()
    local null_ls = require "null-ls"
    null_ls.setup {
      on_attach = require("pokerus.lsp").on_attach,
      sources = {
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.prettier.with {
          filetypes = { "html", "json", "yaml", "markdown" }
        },
        null_ls.builtins.diagnostics.shellcheck,
        null_ls.builtins.formatting.shfmt.with {
          extra_args = { "-i", "2", "-ci" },
        },
        null_ls.builtins.diagnostics.vint,
        null_ls.builtins.diagnostics.statix,
        null_ls.builtins.formatting.asmfmt,
        -- null_ls.builtins.formatting.stylua,
        null_ls.builtins.diagnostics.zsh,
      },
    }
  end,
}
