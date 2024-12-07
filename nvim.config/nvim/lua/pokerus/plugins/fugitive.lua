return {
  "tpope/vim-fugitive",
  event = "VeryLazy",
  cmd = { "G", "Git" },
  config = function()
    require("pokerus").vimsetup("fugitive")
  end,
}
