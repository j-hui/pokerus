-- NOTE: supersedes quicker.nvim, aerial.nvim, and several aspects of Telescope
return {
  "folke/trouble.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" },
  opts = {},
  cmd = "Trouble",
  keys = {
    { "gkq", mode = "n", "<cmd>Trouble qflist<cr>",               desc = "quickfix-list" },
    { "gkl", mode = "n", "<cmd>Trouble loclist<cr>",              desc = "location-list" },
    { "gks", mode = "n", "<cmd>Trouble symbols<cr>",              desc = "symbols" },
    { "gkt", mode = "n", "<cmd>Trouble todo<cr>",                 desc = "todos" },
    { "gkd", mode = "n", "<cmd>Trouble diagnostics<cr>",          desc = "diagnostics" },

    { "gll", mode = "n", "<cmd>Trouble lsp<cr>",                  desc = "lsp-all" },
    { "gls", mode = "n", "<cmd>Trouble lsp_document_symbols<cr>", desc = "lsp-symbols" },
    { "gld", mode = "n", "<cmd>Trouble lsp_definitions<cr>",      desc = "lsp-definitions" },
    { "glr", mode = "n", "<cmd>Trouble lsp_references<cr>",       desc = "lsp-references" },
    { "glc", mode = "n", "<cmd>Trouble lsp_command<cr>",          desc = "lsp-commands" },
    { "gln", mode = "n", "<cmd>Trouble lsp_declarations<cr>",     desc = "lsp-declarations" },
    { "glm", mode = "n", "<cmd>Trouble lsp_implementations<cr>",  desc = "lsp-implementations" },
    { "gli", mode = "n", "<cmd>Trouble lsp_incoming_calls<cr>",   desc = "lsp-incoming-calls" },
    { "glo", mode = "n", "<cmd>Trouble lsp_outgoing_calls<cr>",   desc = "lsp-outgoing-calls" },
    { "glt", mode = "n", "<cmd>Trouble lsp_type_definitions<cr>", desc = "lsp-type-definitions" },
  },
}
