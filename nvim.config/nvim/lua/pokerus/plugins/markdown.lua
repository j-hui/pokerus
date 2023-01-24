return {
  { "preservim/vim-markdown",
    config = function()
      require("pokerus").vimsetup("markdown")
    end,
  },
  { "ellisonleao/glow.nvim" }
}
