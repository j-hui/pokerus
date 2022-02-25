return {
  plug = function(use)
    use {
      "stevearc/dressing.nvim",
      -- requires = {},
      -- Dressing seems to have good enough fallbacks that I shouldn't need to
      -- require telescope or some other picker backend.
      config = function()
        require("dressing").setup {}
      end,
    }
  end,
}
