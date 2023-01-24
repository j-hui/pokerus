return {
  "ruifm/gitlinker.nvim",
  dependencies = { "nvim-lua/plenary.nvim", "ojroques/vim-oscyank" },
  opts = {
    opts = {
      mappings = "<leader>gy",
      action_callback = function(url)
        vim.api.nvim_command("let @\" = '" .. url .. "'")
        vim.fn.OSCYankString(url)
      end,
    },
  },
  keys = {
    { "<leader>gy", desc = "git-yank-url" }
  },
}
