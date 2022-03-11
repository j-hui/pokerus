return {
  plug = function(use)
    use {
      -- disable = true,
      "tranvansang/vim-close-pair",
      -- Move things sideways in lists
      config = function()
        vim.g.close_pair_key = "<C-g>l"

        require("pokerus").imap { ["<C-g>l"] = "close-pair" }

        require("pokerus").imap({
          ["h"] = { "<C-g>l<Left>", "close-pair-inside" },
          ["o"] = { "<C-g>l<Left><CR><Up><C-o>o", "close-pair-next-line" },
        }, { prefix = "<C-g>", noremap = false })
      end,
    }
  end,
}
