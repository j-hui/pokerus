return {
  "Bekaboo/dropbar.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "nvim-treesitter/nvim-treesitter",
  },

  event = "VeryLazy",
  init = function()
    -- Disable dropbar's own lazy loader
    vim.g.loaded_dropbar = true
  end,
  opts = {},
}
