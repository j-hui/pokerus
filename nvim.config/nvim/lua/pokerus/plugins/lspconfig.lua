return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "weilbith/nvim-lsp-smag",
    -- Override tagfunc, use C-] to jump to definition

    "folke/neodev.nvim",
    -- Nvim lua development

    "folke/lsp-colors.nvim",
    -- Creates missing LSP diagnostics highlight groups

    "hrsh7th/cmp-nvim-lsp",
    -- Completion suggestions
  },
  config = function()
    local split, glob, stdpath, substitute = vim.fn.split, vim.fn.glob, vim.fn.stdpath, vim.fn.substitute

    local lspconfigs = split(glob((stdpath "config") .. "/lua/pokerus/lspconfig/*"))

    local capabilities = vim.tbl_deep_extend("force",
      vim.lsp.protocol.make_client_capabilities(),
      require('cmp_nvim_lsp').default_capabilities()
    )

    for _, lsp in ipairs(lspconfigs) do
      lsp = substitute(lsp, "^.*/", "", "")
      lsp = substitute(lsp, "\\.lua$", "", "")

      local cfg = require("pokerus.lspconfig." .. lsp)

      if cfg then
        cfg = cfg == true and {} or cfg
        require("lspconfig")[lsp].setup(vim.tbl_extend("keep", cfg, {
          on_attach = require("pokerus.lsp").on_attach,
          capabilities = capabilities,
        }))
      end
    end
  end,
}
