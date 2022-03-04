return {
  plug = function(use)
    use {
      "folke/trouble.nvim",
      -- Summarize diagnostics in document
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        require("trouble").setup {}

        require("pokerus").nmap({
          ["."] = {
            "lsp-document-diagnostics",
            "<cmd>TroubleToggle lsp_document_diagnostics<CR>",
          },
          [","] = {
            "lsp-workspace-diagnostics",
            "<cmd>TroubleToggle lsp_workspace_diagnostics<CR>",
          },
        }, { prefix = "<leader>l", noremap = true, silent = true })
      end,
    }
  end,
}
