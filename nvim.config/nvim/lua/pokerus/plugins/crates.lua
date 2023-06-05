return {
  "saecki/crates.nvim",
  event = { "BufRead Cargo.toml" },
  dependencies = {
    "nvim-lua/plenary.nvim",
    "hrsh7th/nvim-cmp",
    "jose-elias-alvarez/null-ls.nvim",
  },
  config = function()
    require("crates").setup {
      null_ls = {
        enabled = true,
        name = "crates.nvim",
      },
    }
    require("cmp").setup.buffer({ sources = { { name = "crates" } } })
  end,
}
