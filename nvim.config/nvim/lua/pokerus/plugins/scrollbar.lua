return {
  "petertriho/nvim-scrollbar",
  dependencies = {
    "lewis6991/gitsigns.nvim",
  },
  event = "VeryLazy",
  config = true,
  opt = {
    throttle_ms = 1000,
    hide_if_all_visible = true,
    show_in_active_only = true,
    folds = 1000,      -- (false|number) don't handle folds if line count exceeds this
    max_lines = false, -- (false|number) don't render if line count exceeds this
    handlers = {
      cursor = true,
      diagnostic = true,
      gitsigns = true,
    },
  },
}
