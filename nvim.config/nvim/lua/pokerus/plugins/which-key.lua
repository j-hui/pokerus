return {
  "folke/which-key.nvim",
  opts = {
    spelling = { enabled = true, },
    hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ ", "<Plug>", "<plug>", },
    operators = {
      gc = "comment-line",
      gb = "comment-block",
      s = "substitute",
    },
  },
  config = function(_, opts)
    local wk = require("which-key")
    wk.setup(opts)
    wk.register({
      ["<leader>l"] = { name = "lsp" },
      ["<leader>g"] = { name = "git" },
      ["<leader>x"] = { name = "exec" },
    }, { mode = "n" })
    wk.register({
      ["<leader>l"] = { name = "lsp" },
      ["<leader>g"] = { name = "git" },
      ["<leader>x"] = { name = "exec" },
    }, { mode = "v" })
  end
}
