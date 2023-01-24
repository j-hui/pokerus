return {
  "folke/trouble.nvim",
  cmd = { "TroubleToggle" },
  -- Summarize diagnostics in document
  dependencies = { "neovim/nvim-lspconfig" },
  config = true,
  keys = {
    { "<leader>l.", "<cmd>TroubleToggle lsp_document_diagnostics<CR>", mode = "n", desc = "lsp-document-diagnostics" },
    { "<leader>l,", "<cmd>TroubleToggle lsp_workspace_diagnostics<CR>", mode = "n", desc = "lsp-workspace-diagnostics" },
  },
}
