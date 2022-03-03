return {
  plug = function(use)
    use {
      "folke/trouble.nvim",
      -- Summarize diagnostics in document
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        require("trouble").setup {}
      end,
    }
  end,
}
