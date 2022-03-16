local M = {}

function M.plug(use)
  use {
    "olimorris/persisted.nvim",
    opt = true,
    cmd = { "SessionLoad", "SessionLoadLast" },
    config = function()
      require("persisted").setup {
        use_git_branch = true,
      }
    end,
  }
end

return M
