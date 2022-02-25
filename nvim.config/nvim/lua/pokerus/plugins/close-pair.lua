return {
  plug = function(use)
    use {
      "tranvansang/vim-close-pair",
      -- Move things sideways in lists
      config = function()
        vim.g.close_pair_key = "<C-]>"

        require("pokerus").imap { ["<C-]>"] = "close-pair" }

        require("pokerus").imap({
          ["]"] = { "<C-]><Left>", "close-pair-inside" },
          ["o"] = { "<C-]><Left><CR><Up><C-o>o", "close-pair-next-line" },
        }, { prefix = "<C-g>", noremap = false })
      end,
    }
  end,
}
