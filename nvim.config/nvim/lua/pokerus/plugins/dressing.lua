return {
  "stevearc/dressing.nvim",
  -- dependencies = {},
  -- Dressing seems to have good enough fallbacks that I shouldn't need to
  -- require telescope or some other picker backend.
  event = "VeryLazy",
  config = function()
    require("dressing").setup {}
  end,
}
