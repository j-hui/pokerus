return {
  "https://git.sr.ht/~whynothugo/lsp_lines.nvim",
  lazy = false,
  keys = {
    { "<leader>ll", function() require("lsp_lines").toggle() end, desc = "lsp-lines-toggle" },
  },
  config = function()
    require("lsp_lines").setup()
    vim.diagnostic.config {
      virtual_lines = false,
      virtual_text = false,
    }
  end,
}
