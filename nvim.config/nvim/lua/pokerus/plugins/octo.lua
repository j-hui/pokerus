return {
  plug = function(use)
    use {
      "pwntester/octo.nvim",
      opt = true,
      event = "VimEnter",
      requires = {
        "nvim-lua/plenary.nvim",
        "nvim-telescope/telescope.nvim",
        "kyazdani42/nvim-web-devicons",
      },
      config = function()
        require("octo").setup()
      end,
    }
  end,
}
