local M = {}

M.prefixes = {}

function M.add_prefix(prefix, name)
  M.prefixes[prefix] = name
end

-- The obvious prefixes
M.add_prefix("<leader>l", "lsp")
M.add_prefix("<leader>s", "shell")
M.add_prefix("<leader>g", "git")
M.add_prefix("<leader>/", "search")
M.add_prefix("<leader>t", "test")

local nvim_use_virtual_text = true

local function diagnostics_toggle()
  local virtual_text_config = {
    severity = {
      min = vim.diagnostic.severity.WARN,
    }
  }
  if nvim_use_virtual_text then
    vim.diagnostic.config({
      severity_sort = true,
      virtual_text = virtual_text_config
    })
  else
    vim.diagnostic.config({
      severity_sort = true,
      virtual_text = false
    })
  end
  nvim_use_virtual_text = not nvim_use_virtual_text
end

function M.setup()
  diagnostics_toggle() -- call this to configure diagnostics

  local function goto_next_diagnostic()
    vim.diagnostic.goto_next({ severity = { min = vim.diagnostic.severity.WARN } })
  end

  local function goto_prev_diagnostic()
    vim.diagnostic.goto_prev({ severity = { min = vim.diagnostic.severity.WARN } })
  end

  vim.keymap.set("n", "]d", goto_next_diagnostic, { desc = "diagnostic-next", silent = true })
  vim.keymap.set("n", "[d", goto_prev_diagnostic, { desc = "diagnostic-prev", silent = true })

  vim.keymap.set("n", "<leader>d", vim.diagnostic.open_float, { desc = "diagnostic-show", silent = true })
  vim.keymap.set("n", "<leader>D", diagnostics_toggle, { desc = "diagnostic-toggle", silent = true })

  vim.keymap.set("n", "<leader>+", ":lua ", { desc = "lua-run" })
  vim.keymap.set("n", "<leader>=", ":lua =()<left>", { desc = "lua-eval" })

  vim.keymap.set("n", "g-", ":edit %:h<CR>", { desc = "open-.." })
  vim.keymap.set("n", "g=", ":edit .<CR>", { desc = "open-cwd" })

  vim.keymap.set("n", "]t", "<cmd>tnext<cr>", { desc = "tab-next", silent = true })
  vim.keymap.set("n", "[t", "<cmd>tprev<cr>", { desc = "tab-prev", silent = true })

  vim.keymap.set("n", "]q", "<cmd>cnext<cr>", { desc = "quickfix-next", silent = true })
  vim.keymap.set("n", "[q", "<cmd>cprev<cr>", { desc = "quickfix-prev", silent = true })
end

return M
