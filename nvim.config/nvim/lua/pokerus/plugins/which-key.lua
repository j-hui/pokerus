return {
  plug = function(use)
    use {
      "folke/which-key.nvim",
      config = function()
        require("which-key").setup {
          spelling = {
            enabled = true,
          },
          hidden = {
            "<silent>",
            "<cmd>",
            "<Cmd>",
            "<CR>",
            "call",
            "lua",
            "^:",
            "^ ",
            "<Plug>",
            "<plug>",
          },
        }
        require("pokerus").setup_vim_maps()
      end,
    }
  end,
}
