local M = {}

local function nmap(...)
  vim.keymap.set("n", ...)
end

local function xmap(...)
  vim.keymap.set("x", ...)
end

function M.on_attach(_, bufnr)
  local lsp = vim.lsp.buf
  local function lsp_keymap(desc)
    return {
      desc = "lsp-" .. desc,
      buffer = bufnr,
      silent = true,
    }
  end

  nmap("K", lsp.hover, lsp_keymap "hover")

  nmap("<leader>lk", lsp.hover, lsp_keymap "hover")
  nmap("<leader>ld", lsp.definition, lsp_keymap "goto-definition")
  nmap("<leader>le", lsp.declaration, lsp_keymap "goto-declaration")
  nmap("<leader>li", lsp.implementation, lsp_keymap "goto-implementation")
  nmap("<leader>lt", lsp.type_definition, lsp_keymap "goto-typedef")
  nmap("<leader>l=", lsp.references, lsp_keymap "references")
  nmap("<leader>la", lsp.code_action, lsp_keymap "code-action")
  nmap("<leader>lr", lsp.rename, lsp_keymap "rename")

  xmap("<leader>la", lsp.code_action, lsp_keymap "code-action")

  nmap("<leader>lx", vim.lsp.codelens.run, lsp_keymap "codelens")

  -- vim.cmd [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
  -- ^NOTE: causes weird issue sometimes but that can be ignored

  vim.api.nvim_create_user_command("Fmt", function() vim.lsp.buf.format { async = true } end, { desc = "format" })
  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")

end

-- TODO: try to decouple lspconfig for each filetype.
M.lspconfig = {
  "neovim/nvim-lspconfig",
  dependencies = {
    "weilbith/nvim-lsp-smag",
    -- Override tagfunc, use C-] to jump to definition

    "folke/lsp-colors.nvim",
    -- Creates missing LSP diagnostics highlight groups

    "hrsh7th/cmp-nvim-lsp",
    -- Insertion
  },
  lazy = true,
}

function M.setup(lsp, cfg)
  require("lspconfig")[lsp].setup(vim.tbl_extend("keep", cfg, {
    on_attach = M.on_attach,
    capabilities = require("cmp_nvim_lsp").default_capabilities(
      vim.lsp.protocol.make_client_capabilities()
    ),
  }))
end

return M
