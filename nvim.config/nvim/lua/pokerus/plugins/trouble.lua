return {
  plug = function(use)
    use {
      "folke/trouble.nvim",
      opt = true,
      cmd = { "TroubleToggle" },
      -- Summarize diagnostics in document
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        require("trouble").setup {}

        require("pokerus").nmap({
          ["."] = {
            "<cmd>TroubleToggle lsp_document_diagnostics<CR>",
            "lsp-document-diagnostics",
          },
          [","] = {
            "<cmd>TroubleToggle lsp_workspace_diagnostics<CR>",
            "lsp-workspace-diagnostics",
          },
        }, { prefix = "<leader>l", noremap = true, silent = true })
      end,
    }
  end,
}
