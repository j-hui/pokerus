return {
  "folke/which-key.nvim",

  config = function()
    local wk = require("which-key")
    wk.setup()

    for prefix, name in pairs(require("pokerus.keybinds").prefixes) do
      wk.add({ prefix, group = name })
    end
  end,
}
