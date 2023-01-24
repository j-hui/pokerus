return {
  "monaqa/dial.nvim",
  keys = {
    { "+", mode = "n" },
    { "-", mode = "n" },
    { "+", mode = "x" },
    { "-", mode = "x" },
    { "g+", mode = "x" },
    { "g-", mode = "x" },
  },
  config = function()
    local dm = require("dial.map")
    vim.keymap.set("n", "+", dm.inc_normal(), { desc = "dial-increment" })
    vim.keymap.set("n", "-", dm.dec_normal(), { desc = "dial-decrement" })
    vim.keymap.set("v", "+", dm.inc_visual(), { desc = "dial-increment" })
    vim.keymap.set("v", "-", dm.dec_visual(), { desc = "dial-decrement" })
    vim.keymap.set("v", "g+", dm.inc_gvisual(), { desc = "dial-g-increment" })
    vim.keymap.set("v", "g-", dm.dec_gvisual(), { desc = "dial-g-decrement" })
  end,
}
