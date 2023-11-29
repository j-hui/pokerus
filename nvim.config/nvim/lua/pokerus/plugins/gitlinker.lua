return {
  "ruifm/gitlinker.nvim",
  dependencies = { "nvim-lua/plenary.nvim", "ojroques/vim-oscyank" },
  opts = {
    opts = {
      action_callback = function(url)
        vim.api.nvim_command("let @\" = '" .. url .. "'")
        vim.fn.OSCYank(url)
      end,
    },
    mappings = "<leader>gy",
  },
  keys = {
    { "<leader>gy", desc = "git-yank-url", mode = { "n", "v" } }
  },
}
