local M = {}

local lsp_buf_apis = {
  { "hover",            key = { "K", "<leader>lk" }, },
  { "definition",       key = "<leader>ld",          opts = { reuse_win = true }, },
  { "declaration",      key = "<leader>le",          opts = { reuse_win = true }, },
  { "type_definition",  key = "<leader>lt",          opts = { reuse_win = true }, },
  { "implementation",   key = "<leader>lm",          opts = { reuse_win = true }, },
  { "code_action",      key = "<leader>la",          mode = { "n", "x" } },
  { "references",       key = "<leader>ln", },
  { "typehierarchy",    key = "<leader>lh", },
  { "incoming_calls",   key = "<leader>li",          desc = "incoming-calls", },
  { "outgoing_calls",   key = "<leader>lo",          desc = "outgoing-calls", },
  { "document_symbol",  key = "<leader>ls",          desc = "document-symbols", },
  { "workspace_symbol", key = "<leader>lS",          desc = "workspace-symbols", },
  { "rename",           key = "<leader>lr", },
  { "format",           opts = { async = true }, },
  { "command", },
}

for _, api in ipairs(lsp_buf_apis) do
  api.mode = api.mode or { "n" }
  if type(api.mode) ~= "table" then
    api.mode = { api.mode }
  end

  if api.key and type(api.key) ~= "table" then
    api.key = { api.key }
  end

  M[api[1]] = function()
    vim.lsp.buf[api[1]](api.opts or {})
  end
end

function M.on_attach(_, bufnr)
  -- vim.cmd [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
  -- ^ causes weird issue sometimes but that can be ignored

  -- vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
end

-- TODO codelens

function M.setup()
  for _, api in ipairs(lsp_buf_apis) do
    for _, key in ipairs(api.key or {}) do
      for _, mode in ipairs(api.mode) do
        vim.keymap.set(mode, key, M[api[1]], {
          desc = "lsp-" .. (api.desc or api[1]),
          silent = true
        })
      end
    end
  end
  vim.api.nvim_create_user_command("Fmt", M.format, { desc = "format" })
end

return M
