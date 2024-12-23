return {
  "rcarriga/nvim-notify",
  config = function()
    -- vim.notify = require("notify")
    vim.keymap.set("n", "<leader><bs>", require("notify").dismiss, { desc = "notifications-clear" })
  end,
}
