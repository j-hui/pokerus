return {
  plug = function(use)
    use {
      "folke/twilight.nvim",
      requires = { "nvim-treesitter/nvim-treesitter" },
      config = function()
        require("twilight").setup {}
      end,
    }
  end,
}
