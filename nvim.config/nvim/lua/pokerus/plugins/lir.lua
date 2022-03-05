return {
  plug = function(use)
    use {
      "tamago324/lir.nvim",
      requires = {
        "nvim-lua/plenary.nvim",
        "kyazdani42/nvim-web-devicons",
        "tamago324/lir-git-status.nvim",
      },
      config = function()
        require("lir").setup {}
        require("lir.git_status").setup { show_ignored = false }
      end,
    }
  end,
}
