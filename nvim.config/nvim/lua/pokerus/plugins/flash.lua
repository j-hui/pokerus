return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {
    modes = {
      search = {
        enabled = false, -- Rely on "," to do epic searching
      },
      char = {
        keys = { "f", "F", "t", "T", [";"] = "L", [","] = "H" },
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
      desc = "Flash",
    },
    {
      "!", -- Motion/jump that targets treesitter nodes
      mode = { "n", "o", "x" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash Treesitter",
    },
    {
      "r", -- Motion in remote location/buffer (with automatic return)
      mode = "o",
      function()
        require("flash").remote()
      end,
      desc = "Remote Flash",
    },
  },
}
