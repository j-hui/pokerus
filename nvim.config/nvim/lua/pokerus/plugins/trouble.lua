return {
  "folke/trouble.nvim",
  -- NOTE: supersedes quicker.nvim, aerial.nvim, and several aspects of Telescope
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {
    auto_preview = false, -- Auto preview slows everything down. Instead, use `p` to preview in the Trouble pane.
    throttle = {
      follow = { ms = 500, debounce = true },
      preview = { ms = 500, debounce = true },
    },
  },
  cmd = "Trouble",
  keys = {
    { "gkq", mode = "n", "<cmd>Trouble qflist toggle<cr>",               desc = "quickfix-list" },
    { "gkl", mode = "n", "<cmd>Trouble loclist toggle<cr>",              desc = "location-list" },
    { "gks", mode = "n", "<cmd>Trouble symbols toggle<cr>",              desc = "symbols" },
    { "gkt", mode = "n", "<cmd>Trouble todo toggle<cr>",                 desc = "todos" },
    { "gkd", mode = "n", "<cmd>Trouble diagnostics toggle<cr>",          desc = "diagnostics" },

    { "gll", mode = "n", "<cmd>Trouble lsp toggle<cr>",                  desc = "lsp-all" },
    { "gls", mode = "n", "<cmd>Trouble lsp_document_symbols toggle<cr>", desc = "lsp-symbols" },
    { "gld", mode = "n", "<cmd>Trouble lsp_definitions toggle<cr>",      desc = "lsp-definitions" },
    { "glr", mode = "n", "<cmd>Trouble lsp_references toggle<cr>",       desc = "lsp-references" },
    { "glc", mode = "n", "<cmd>Trouble lsp_command toggle<cr>",          desc = "lsp-commands" },
    { "gln", mode = "n", "<cmd>Trouble lsp_declarations toggle<cr>",     desc = "lsp-declarations" },
    { "glm", mode = "n", "<cmd>Trouble lsp_implementations toggle<cr>",  desc = "lsp-implementations" },
    { "gli", mode = "n", "<cmd>Trouble lsp_incoming_calls toggle<cr>",   desc = "lsp-incoming-calls" },
    { "glo", mode = "n", "<cmd>Trouble lsp_outgoing_calls toggle<cr>",   desc = "lsp-outgoing-calls" },
    { "glt", mode = "n", "<cmd>Trouble lsp_type_definitions toggle<cr>", desc = "lsp-type-definitions" },
  },
  init = function()
    require("pokerus.keybinds").add_prefix("gk", "trouble")
    require("pokerus.keybinds").add_prefix("gl", "trouble-lsp")
  end,
}
