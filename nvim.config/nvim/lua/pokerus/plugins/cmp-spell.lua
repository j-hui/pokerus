return {
  "f3fora/cmp-spell",
    dependencies = {
      "hrsh7th/nvim-cmp",
      "nvim-treesitter/nvim-treesitter",
    },
  init = function()
    table.insert(require("pokerus.plugins.cmp").sources, {
      {
        name = "spell",
        option = {
          enable_in_context = function(params)
            return require('cmp.config.context').in_treesitter_capture('spell')
          end,
        }
      }
    })
  end,
}
