return {
  "stevearc/overseer.nvim",
  opts = {
  },
  keys = {
    { "gj",               "<cmd>OverseerToggle<CR>",  desc = "overseer-open" },
    { "gJ",               "<cmd>OverseerToggle!<CR>", desc = "overseer-open!" },
    { "<leader>j<space>", "<cmd>OverseerCmdRun<CR>",  desc = "overseer-cmd" },
    { "<leader>jj",       "<cmd>OverseerRun<CR>",     desc = "overseer-run" },
  },
}
