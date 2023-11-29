return {
  "mrcjkb/haskell-tools.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "akinsho/toggleterm.nvim",
    "kevinhwang91/nvim-ufo",
  },
  ft = { "haskell", "cabal" },
  init = function()
    -- haskell-tools apparently no longer wants you to call .setup()
    vim.g.haskell_tools = {
      hls = {
        on_attach = function(client, bufnr)
          require("pokerus.lsp").on_attach(client, bufnr)
        end,
        settings = {
          haskell = {
            formattingProvider = "fourmolu",
            plugin = {
              stan = { globalOn = false },
            },
          },
        },
      },
      repl = { handler = "toggleterm" },
    }
  end,
}
