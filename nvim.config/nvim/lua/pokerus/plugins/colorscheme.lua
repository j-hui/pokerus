return {
  {
    "nyoom-engineering/oxocarbon.nvim",
    lazy = true, -- use this as main theme
    priority = 1000, -- load this before all the other plugins
    config = function()
      vim.cmd([[colorscheme oxocarbon]])
    end,
  },
  {
    "folke/tokyonight.nvim",
    lazy = false,
    priority = 1000, -- load this before all the other plugins
    config = function()
      require("tokyonight").setup({
        style = "night",
        styles = {
          keywords = { italic = false }
        },
        sidebars = { "qf", "vista_kind", "terminal", "packer" }
      })
      vim.cmd([[colorscheme tokyonight]])
    end,
  },
  {
    "projekt0n/github-nvim-theme",
    lazy = true,
    priority = 1000, -- load this before all the other plugins
    opts = {
      keyword_style = "bold",
    },
  },
}
