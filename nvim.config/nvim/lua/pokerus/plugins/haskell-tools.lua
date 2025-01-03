local hls_opts = require("pokerus.lspconfig.hls").opts or {}

return {
  "mrcjkb/haskell-tools.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  ft = { "haskell", "cabal" },
  init = function()
    -- haskell-tools apparently no longer wants you to call .setup()
    vim.g.haskell_tools = {
      hls = {
        on_attach = function(client, bufnr)
          require("pokerus.lsp").on_attach(client, bufnr)
        end,
        settings = hls_opts.settings,
      },
    }
  end,
}
