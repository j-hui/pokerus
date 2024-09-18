return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {
    modes = {
      search = {
        enabled = false, -- Rely on "," to do epic searching
      },
      char = {
        -- NOTE: H and L are remapped to jump forward and backward
        keys = { "f", "F", "t", "T", [";"] = "L", [","] = "H" },
      },
      treesitter = {
        highlight = {
          backdrop = true,
        }
      },
    },
  },
  keys = {
    {
      ",", -- Motion/jump that combines search and easymotion-like keys
      mode = { "n", "x", "o" },
      function()
        -- default options: exact mode, multi window, all directions, with a backdrop
        require("flash").jump { search = { mode = "fuzzy" } }
      end,
      desc = "flash-search",
    },
    {
      "q", -- Motion/jump that targets treesitter nodes
      mode = { "o", "x" },
      function()
        require("flash").treesitter()
      end,
      desc = "flash-treesitter",
    },
    {
      "o", -- Motion in remote location/buffer (with automatic return)
      mode = "o",
      function()
        require("flash").remote()
      end,
      desc = "flash-remote",
    },
  },
}
