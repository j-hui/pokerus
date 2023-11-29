return {} or {
  "rcarriga/nvim-notify",
  config = function()
    vim.notify = require "notify"
    vim.keymap.set("n", "<leader>q", vim.notify.dismiss, { desc = "notifications-clear" })
  end,
}
