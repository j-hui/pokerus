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
      ",",
      mode = { "n", "x", "o" },
      function()
        -- default options: exact mode, multi window, all directions, with a backdrop
        require("flash").jump { search = { mode = "fuzzy" } }
      end,
      desc = "Flash",
    },
    {
      "!",
      mode = { "n", "o", "x" },
      function()
        require("flash").treesitter()
      end,
      desc = "Flash Treesitter",
    },
    {
      "r",
      mode = "o",
      function()
        require("flash").remote()
      end,
      desc = "Remote Flash",
    },
  },
}
