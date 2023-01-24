return {
  "stevearc/aerial.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
    "nvim-treesitter/nvim-treesitter",
  },
  opts = {
    manage_folds = false,
    backends = {
      ["_"] = { "lsp", "treesitter" },
      ["bib"] = { "treesitter" },
    },
  },
  cmd = { "AerialToggle", "AerialOpen" },
  keys = {
    { "<leader>lo", "<cmd>AerialToggle!<CR>", mode = "n", desc = "lsp-outline" }
  }
}
