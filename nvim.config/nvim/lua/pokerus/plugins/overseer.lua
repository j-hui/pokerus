return {
  "stevearc/overseer.nvim",
  event = "VeryLazy",
  opts = {
  },
  keys = {
    { "gj",         "<cmd>OverseerToggle<CR>",  desc = "overseer-open" },
    { "gJ",         "<cmd>OverseerToggle!<CR>", desc = "overseer-open!" },
    { "<leader>jc", "<cmd>OverseerRunCmd<CR>",  desc = "overseer-cmd" },
    { "<leader>jr", "<cmd>OverseerRun<CR>",     desc = "overseer-run" },
  },
}
