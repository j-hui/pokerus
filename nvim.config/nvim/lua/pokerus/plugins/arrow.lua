return {
  "otavioschwanck/arrow.nvim",
  dependencies = {
    { "nvim-tree/nvim-web-devicons" },
  },
  opts = {
    show_icons = true,
    leader_key = "<C-t>",    -- Recommended to be a single key
    buffer_leader_key = "m", -- Per Buffer Mappings
    save_key = "git_root",
    mappings = {
      edit = "e",
      delete_mode = "d",
      clear_all_items = "C",
      toggle = "g", -- save/remove
      open_vertical = "s",
      open_horizontal = "x",
      quit = "q",
      next_item = "]",
      prev_item = "["
    },
  }
}
