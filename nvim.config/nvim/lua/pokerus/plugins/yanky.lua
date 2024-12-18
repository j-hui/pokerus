return {
  "gbprod/yanky.nvim",
  event = "VeryLazy",
  cmd = { "YankyClearHistory", "YankyRingHistory" },
  keys = {
    { mode = { "n", "x" }, "p",         "<Plug>(YankyPutAfter)",                            desc = "put-after" },
    { mode = { "n", "x" }, "P",         "<Plug>(YankyPutBefore)",                           desc = "put-before" },
    { mode = { "n", "x" }, "gp",        "<Plug>(YankyGPutAfter)",                           desc = "gput-after" },
    { mode = { "n", "x" }, "gP",        "<Plug>(YankyGPutBefore)",                          desc = "gput-before" },

    { mode = "n",          "[y",        "<Plug>(YankyCycleForward)",                        desc = "yanky-history-forward" },
    { mode = "n",          "]y",        "<Plug>(YankyCycleBackward)",                       desc = "yanky-history-backward" },
    { mode = "n",          "<leader>y", "<cmd>YankyRingHistory<CR>",                        desc = "yanky-history" },
    { mode = "n",          "<leader>y", "<cmd>YankyClearHistory<CR>",                       desc = "yanky-clear-history" },

    { mode = { "o", "x" }, "Y",         function() require("yanky.textobj").last_put() end, desc = "last-put" },
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
    textobj = {
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
  end
}
