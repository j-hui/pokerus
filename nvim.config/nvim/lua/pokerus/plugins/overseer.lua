return {
  "stevearc/overseer.nvim",
  event = "VeryLazy",
  opts = {},
  config = true,
  keys = {
    { "<leader>ss", "<cmd>OverseerToggle<CR>",  desc = "overseer-open" },
    { "<leader>sc", "<cmd>OverseerRunCmd<CR>",  desc = "overseer-cmd" },
    { "<leader>sr", "<cmd>OverseerRun<CR>",     desc = "overseer-run" },
  },
}
