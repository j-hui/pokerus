return {
  "debugloop/telescope-undo.nvim",
  dependencies = {
    "nvim-telescope/telescope.nvim",
  },
  keys = {
    { "<leader>u", require("pokerus.plugins.telescope").extension "undo", desc = "telescope-undo" },
  },
  init = function()
    local tele = require("pokerus.plugins.telescope")
    tele.extensions.undo = {
      side_by_side = true,
    }
  end,
  config = function()
    require("telescope").load_extension "undo"
  end
}
