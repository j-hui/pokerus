return {
  "mrcjkb/haskell-tools.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
    "akinsho/toggleterm.nvim",
  },
  ft = { "haskell", "cabal" },
  config = function()
    require('haskell-tools').setup {
      hls = {
        on_attach = function()
          require("pokerus.lsp").on_attach()
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
