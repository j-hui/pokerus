return {
  "rcarriga/nvim-notify",
  keys = {
    ---@diagnostic disable-next-line: missing-parameter
    { "<leader><BS>", function() require("notify").dismiss() end, desc = "notify-clear" },
  },
  lazy = false,
  config = function()
    require("pokerus.callback").filetype("notify", function()
      vim.keymap.set("n", "q", "<cmd>quit<cr>", { desc = "close", silent = true, })
    end)
  end,
}
