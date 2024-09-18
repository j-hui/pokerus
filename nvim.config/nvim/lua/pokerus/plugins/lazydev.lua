return {
  {
    "folke/lazydev.nvim",
    ft = "lua", -- only load on lua files
    dependencies = { "hrsh7th/nvim-cmp" },
    init = function()
      table.insert(require("pokerus.plugins.cmp").sources, {
        name = "lazydev",
        group_index = 0, -- set group index to 0 to skip loading LuaLS completions
      })
    end,
    opts = {
      library = {
        -- See the configuration section for more details
        -- Load luvit types when the `vim.uv` word is found
        { path = "luvit-meta/library", words = { "vim%.uv" } },
      },
    },
  },
  { "Bilal2453/luvit-meta", lazy = true }, -- optional `vim.uv` typings
}
