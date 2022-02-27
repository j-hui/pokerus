return {
  plug = function(use)
    use {
      "preservim/vim-markdown",
      config = require("pokerus.plugins.vimbridge").vimsetup "markdown",
    }
    use "ellisonleao/glow.nvim"
  end,
}
