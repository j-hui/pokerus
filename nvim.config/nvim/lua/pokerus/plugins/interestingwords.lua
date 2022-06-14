local M = {}

function M.config()
  vim.g.interestingWordsDefaultMappings = 0

  require("pokerus").nmap {
    ["<leader>i"] = {
      function()
        vim.fn.InterestingWords "n"
      end,
      "mark-interesting",
    },
    ["<leader>I"] = {
      function()
        vim.cmd [[noh]]
        vim.fn.UncolorAllWords()
      end,
      "unmark-all",
    },
    ["]i"] = {
      function()
        vim.fn.WordNavigation(1)
      end,
      "next-interesting",
    },
    ["[i"] = {
      function()
        vim.fn.WordNavigation(0)
      end,
      "prev-interesting",
    },
  }

  require("pokerus").xmap {
    ["<leader>i"] = {
      function()
        vim.fn.InterestingWords "n"
      end,
      "mark-interesting",
    },
  }
end

function M.plug(use)
  use {
    "lfv89/vim-interestingwords",
    config = function()
      require("pokerus.plugins.interestingwords").config()
    end,
  }
end

return M
