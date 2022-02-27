local M = {}

function M.plug(use)
  use {
    "tpope/vim-fugitive",
    config = function()
      require("pokerus.plugins.vimbridge").vimsetup "fugitive"
      require("pokerus").nmap({
        name = "git",
        d = { "<cmd>Gdiffsplit<CR>", "git-diff-split" },
        D = { "<cmd>Git diff --cached<CR>", "git-diff-cached" },
        p = { "<cmd>Git pull<CR>", "git-pull" },
        P = { "<cmd>Git push<CR>", "git-push" },
        c = { "<cmd>Git commit<CR>", "git-commit" },
        s = { "<cmd>Git<CR>", "git-status" },

        -- l = { "<cmd>Gclog<CR>", "git-log" },
        -- use telescope instead
      }, { prefix = "<leader>g" })
    end,
  }
end

return M
