return {
  plug = function(use)
    use {
      "lfv89/vim-interestingwords",
      config = function()
        vim.g.interestingWordsDefaultMappings = 0

        require("pokerus").nmap {
          ["<leader>m"] = {
            function()
              vim.fn.InterestingWords "n"
            end,
            "mark-interesting",
          },
          ["<leader>M"] = {
            function()
              vim.cmd [[noh]]
              vim.fn.UncolorAllWords()
            end,
            "unmark-all",
          },
          ["]m"] = {
            function()
              vim.fn.WordNavigation(1)
            end,
            "next-interesting",
          },
          ["[m"] = {
            function()
              vim.fn.WordNavigation(0)
            end,
            "prev-interesting",
          },
        }

        require("pokerus").vmap {
          ["<leader>m"] = {
            function()
              vim.fn.InterestingWords "n"
            end,
          },
        }
      end,
    }
  end,
}
