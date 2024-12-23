local M = {
  "folke/trouble.nvim",
  -- NOTE: supersedes quicker.nvim, aerial.nvim, and several aspects of Telescope
  dependencies = { "nvim-tree/nvim-web-devicons" },
  cmd = "Trouble",
  event = "VeryLazy",
  opts = {
    focus = true,
    auto_preview = false, -- Auto preview slows everything down. Instead, use `p` to preview in the Trouble pane.
    throttle = {
      follow = { ms = 500, debounce = true },
      preview = { ms = 500, debounce = true },
    },
    keys = {
      ["p"] = "preview",
      ["Q"] = "fold_toggle",
      ["<c-x>"] = "jump_split",
      ["<c-s>"] = "jump_vsplit",
      ["<c-v>"] = false,
    },
  },
}

local function trouble(fn, args)
  return function()
    require("trouble")[fn](args)
  end
end

function M.init()
  require("pokerus.keybinds").add_prefix("<leader>w", "trouble")
end

M.keys = {
  { "]w",            desc = "trouble-next",        trouble("next") },
  { "[w",            desc = "trouble-prev",        trouble("prev") },
  { "gw",            desc = "trouble-jump",        trouble("jump_only") },
  { "gW",            desc = "trouble-jump",        trouble("jump_close") },
  { "<leader>ww",    desc = "trouble-focus",       trouble("focus") },
  { "<leader>w<BS>", desc = "trouble-close",       trouble("close") },
  { "<leader>wq",    desc = "trouble-qflist",      trouble("toggle", { mode = "qflist" }) },
  { "<leader>ws",    desc = "trouble-symbols",     trouble("toggle", { mode = "symbols" }) },
  { "<leader>wt",    desc = "trouble-todos",       trouble("toggle", { mode = "todos" }) },
  { "<leader>wd",    desc = "trouble-diagnostics", trouble("toggle", { mode = "diagnostics" }) },
  { "<leader>wl",    desc = "trouble-lsp",         trouble("toggle", { mode = "lsp" }) },
}

function M.config(_, opts)
  local tr = require("trouble")
  tr.setup(opts)

  local lsp = require("pokerus.lsp")
  for trouble_mode, lsp_fn in pairs {
    lsp_command = "command",
    lsp_declarations = "declaration",
    lsp_definitions = "definition",
    lsp_document_symbols = "document_symbol",
    lsp_workspace_symbols = "workspace_symbol",
    lsp_implementations = "implementation",
    lsp_incoming_calls = "incoming_calls",
    lsp_outgoing_calls = "outgoing_calls",
    lsp_references = "references",
    lsp_type_definitions = "type_definition",
  } do
    lsp[lsp_fn] = function()
      tr.toggle({ mode = trouble_mode })
    end
  end

  -- Automatically open Trouble quickfix
  vim.api.nvim_create_autocmd("QuickFixCmdPost", {
    callback = function()
      tr.open("qflist")
    end,
  })
end

return M
