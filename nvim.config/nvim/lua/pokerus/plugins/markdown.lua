return {
  plug = function(use)
    use {
      "preservim/vim-markdown",
      config = function()
        require("pokerus").vimsetup("markdown")
      end,
    }
    use "ellisonleao/glow.nvim"
  end,
}
