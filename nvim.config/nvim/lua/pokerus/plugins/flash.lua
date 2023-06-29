return {
  "folke/flash.nvim",
  event = "VeryLazy",
  opts = {
    modes = {
      maps = { [";"] = "L", [","] = "H" },
    },
  },
  keys = {
    {
      ",",
      mode = { "n", "x", "o" },
      function()
        -- default options: exact mode, multi window, all directions, with a backdrop
        require("flash").jump { search = { mode = "fuzzy" }}
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
