local M = {}

function M.plug(use)
  use {
    "projekt0n/github-nvim-theme",
    as = "theme",
    config = function()
      require("github-theme").setup {
        keyword_style = "bold",
      }
    end,
  }
end

return M
