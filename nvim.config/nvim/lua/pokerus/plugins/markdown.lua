return {
  { "preservim/vim-markdown",
    ft = "markdown",
    config = function()
      require("pokerus").vimsetup("markdown")
    end,
  },
  { "ellisonleao/glow.nvim", ft = "markdown" }
}
