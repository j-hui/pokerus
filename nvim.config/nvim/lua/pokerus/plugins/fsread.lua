return {
  "nullchilly/fsread.nvim",
  cmd = {
    "FSRead",
    "FSClear",
    "FSToggle",
  },
  keys = {
    { "<leader>w", "<cmd>FSToggle<CR>", mode = { "n", "v" }, desc = "fsread-toggle" },
    { "<leader>W", "<cmd>FSClear<CR>",  mode = { "n", "v" }, desc = "fsread-toggle" },
  },
}
