local M = {}

local function nmap(...)
  vim.keymap.set("n", ...)
end

local function xmap(...)
  vim.keymap.set("x", ...)
end

local function opt_jump(opt)
  opt = opt or {}
  if opt.jump == "split" then
    vim.cmd [[wincmd s]]
  elseif opt.jump == "vsplit" then
    vim.cmd [[wincmd v]]
  end
end

function M.hover()
  vim.lsp.buf.hover()
end

function M.declaration(opt)
  opt_jump(opt)
  vim.lsp.buf.declaration { reuse_win = true }
end

function M.definition(opt)
  opt_jump(opt)
  vim.lsp.buf.definition { reuse_win = true }
end

function M.typedef(opt)
  opt_jump(opt)
  vim.lsp.buf.type_definition { reuse_win = true }
end

function M.implementation(opt)
  opt_jump(opt)
  vim.lsp.buf.implementation { reuse_win = true }
end

function M.references(opt)
  opt_jump(opt)
  vim.lsp.buf.references()
end

function M.code_action()
  vim.lsp.buf.code_action()
end

function M.rename()
  vim.lsp.buf.rename()
end

function M.codelens()
  vim.lsp.codelens.run()
end

function M.format()
  vim.lsp.buf.format { async = true }
end

function M.on_attach(_, bufnr)
  local function lsp_keymap(desc)
    return {
      desc = "lsp-" .. desc,
      buffer = bufnr,
      silent = true,
    }
  end

  nmap("K", M.hover, lsp_keymap "hover")
  nmap("<leader>lk", M.hover, lsp_keymap "hover")
  nmap("<leader>l=", M.format, lsp_keymap "format")
  nmap("<leader>la", M.code_action, lsp_keymap "code-action")
  nmap("<leader>lr", M.rename, lsp_keymap "rename")
  xmap("<leader>la", M.code_action, lsp_keymap "code-action")
  nmap("<leader>lx", M.codelens, lsp_keymap "codelens")

  for _, km in ipairs {
    { "d", "definition" },
    { "e", "declaration" },
    { "i", "implementation" },
    { "t", "typedef" },
    { "*", "references" },
  } do
    local key, func = km[1], km[2]
    nmap("<leader>l" .. key, M[func], lsp_keymap("goto-" .. func))
    nmap("<leader>lv" .. key, function() M[func] { jump = "vsplit" } end, lsp_keymap("vsplit-" .. func))
    nmap("<leader>ls" .. key, function() M[func] { jump = "split" } end, lsp_keymap("split-" .. func))
  end

  -- vim.cmd [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
  -- ^NOTE: causes weird issue sometimes but that can be ignored

  vim.api.nvim_create_user_command("Fmt", M.format, { desc = "format" })
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
