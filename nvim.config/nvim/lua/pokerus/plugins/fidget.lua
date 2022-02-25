return {
  plug = function(use)
    use {
      "j-hui/fidget.nvim",
      -- Progress handler UI
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        require("fidget").setup {
          debug = {
            logging = true,
          },
        }
      end,
    }
  end,
}
