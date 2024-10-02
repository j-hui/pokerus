-- Superseded by Trouble
return {} or {
  "stevearc/quicker.nvim",
  event = "FileType qf",
  keys = {
    { "gkc", function() require("quicker").toggle() end,                   desc = "toggle-quickfix" },
    { "gkl", function() require("quicker").toggle({ loclist = true }) end, desc = "toggle-loclist" },
  },
  opts = {
    keys = {
      {
        ">",
        function()
          require("quicker").expand({ before = 2, after = 2, add_to_existing = true })
        end,
        desc = "Expand quickfix context",
      },
      {
        "<",
        function()
          require("quicker").collapse()
        end,
        desc = "Collapse quickfix context",
      },
    },
  },
}
