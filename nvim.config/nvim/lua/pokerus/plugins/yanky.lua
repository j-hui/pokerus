return {
  { "gbprod/yanky.nvim",
    dependencies = { "nvim-telescope/telescope.nvim" },
    cmd = { "YankyClearHistory", "YankyRingHistory" },
    keys = {
      { mode = { "n", "x" }, "p", "<Plug>(YankyPutAfter)", desc = "yanky-put-after" },
      { mode = { "n", "x" }, "P", "<Plug>(YankyPutBefore)", desc = "yanky-put-before" },
      { mode = { "n", "x" }, "gp", "<Plug>(YankyGPutAfter)", desc = "yanky-gput-after" },
      { mode = { "n", "x" }, "gP", "<Plug>(YankyGPutBefore)", desc = "yanky-gput-before" },

      { mode = "n", "[p", "<Plug>(YankyCycleForward)", desc = "yanky-forward" },
      { mode = "n", "]p", "<Plug>(YankyCycleBackward)", desc = "yank-backward" },
      { mode = "n", "<leader>p", "<cmd>Telescope yank_history<CR>", desc = "yanky-history" },
    },
    opts = {
      highlight = {
        on_put = true,
        on_yank = true,
        timer = 420,
      },
      preserve_cursor_position = {
        enabled = true,
      },
    },
    config = function(_, opts)
      local function set_yanky_hl()
        vim.api.nvim_set_hl(0, "YankyPut", { link = "DiffAdd" })
        vim.api.nvim_set_hl(0, "YankyYanked", { link = "DiffChange" })
      end

      set_yanky_hl()
      require("pokerus.callback").colorscheme(set_yanky_hl)
      require("yanky").setup(opts)
      require("telescope").load_extension("yank_history")
    end
  },
  { "gbprod/substitute.nvim",
    dependencies = { "gbprod/yanky.nvim" },
    keys = {
      { mode = "n", "s", "<cmd>lua require('substitute').operator()<cr>", desc = "substitute" },
      { mode = "n", "ss", "<cmd>lua require('substitute').line()<cr>", desc = "substitute-line" },
      { mode = "n", "S", "<cmd>lua require('substitute').eol()<cr>", desc = "substitute-eol" },
      { mode = "x", "s", "<cmd>lua require('substitute').visual()<cr>", desc = "substitute" },
    },
    config = function()
      require("substitute").setup({
        on_substitute = require("yanky.integration").substitute(),
      })
    end,
  },
}
