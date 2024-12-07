local function open(opts)
  return function()
    require("neogit").open(opts)
  end
end

local function action(popup, act, args)
  return function()
    require("neogit").action(popup, act, args)()
  end
end

return {
  "NeogitOrg/neogit",
  dependencies = {
    "nvim-lua/plenary.nvim",         -- required
    "sindrets/diffview.nvim",        -- optional - Diff integration
    "nvim-telescope/telescope.nvim", -- optional
  },
  cmd = { "Neogit", "NeogitLog" },
  keys = {
    { "<leader>gc", open { "commit" },                 mode = "n", desc = "git-commit" },
    { "<leader>gg", open { kind = "split_above_all" }, mode = "n", desc = "git-status" },
    { "<leader>gp", open { "pull" },                   mode = "n", desc = "git-pull" },
    { "<leader>gP", open { "push" },                   mode = "n", desc = "git-push" },
    { "<leader>gb", open { "branch" },                 mode = "n", desc = "git-branch" },
    { "<leader>gl", action("log", "log_current"),      mode = "n", desc = "git-log" },
    { "<leader>gL", "<cmd>NeogitLog<CR>",              mode = "n", desc = "git-file-log" },
  },
  opts = {
    graph_style = "unicode",
    mappings = {
      popup = {
        ["l"] = false,
        ["g"] = "LogPopup",
      },
      rebase_editor = {
        ["gj"] = false,
        ["gk"] = false,
        ["J"] = "MoveUp",
        ["K"] = "MoveDown",
      },
    },
  },
  config = true,
}
