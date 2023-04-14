return {} or {
  "LeonHeidelbach/trailblazer.nvim",
  keys = {
    { "mm", "<Cmd>TrailBlazerNewTrailMark<CR>", mode = { "n", "v" }, desc = "trailblazer-mark" },

    { "mb", "<Cmd>TrailBlazerTrackBack<CR>", mode = { "n", "v" }, desc = "trailblazer-back" },
    { "mf", "<Cmd>TrailBlazerMoveToTrailMarkCursor<CR>", mode = { "n", "v" }, desc = "trailblazer-front" },

    { "mp", "<Cmd>TrailBlazerPeekMovePreviousUp<CR>", mode = { "n", "v" }, desc = "trailblazer-prev" },
    { "mn", "<Cmd>TrailBlazerPeekMoveNextDown<CR>", mode = { "n", "v" }, desc = "trailblazer-next" },

    { "mj", "<Cmd>TrailBlazerMoveToNearest 0 down<CR>", mode = { "n", "v" }, desc = "trailblazer-down" },
    { "mk", "<Cmd>TrailBlazerMoveToNearest 0 up<CR>", mode = { "n", "v" }, desc = "trailblazer-up" },

    { "mq", "<Cmd>TrailBlazerDeleteAllTrailMarks<CR>", mode = { "n", "v" }, desc = "trailblazer-clear" },

    { "mp", "<Cmd>TrailBlazerPasteAtLastTrailMark<CR>", mode = { "n", "v" }, desc = "trailblazer-paste" },
    { "mP", "<Cmd>TrailBlazerPasteAtAllTrailMarks<CR>", mode = { "n", "v" }, desc = "trailblazer-paste-all" },

    { "mo", "<Cmd>TrailBlazerToggleTrailMarkList<CR>", mode = { "n", "v" }, desc = "trailblazer-list" },
  },
  config = function()
    require("trailblazer").setup({
      force_mappings = {},
    })
  end,
}
