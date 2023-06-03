return {
  "olimorris/persisted.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  cmd = {
    "SessionToggle",
    "SessionStart",
    "SessionStop",
    "SessionSave",
    "SessionLoad",
    "SessionLoadLast",
    "SessionDelete",
  },
  config = function()
    require("persisted").setup {
      use_git_branch = true,
    }
    require("telescope").load_extension("persisted")
  end,
}
