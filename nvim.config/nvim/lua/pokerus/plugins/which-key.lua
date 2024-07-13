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
    wk.add({
      {
        mode = { "n" },
        { "<leader>l", group = "lsp" },
        { "<leader>g", group = "git" },
        { "<leader>x", group = "exec" },
      },
      {
        mode = { "v" },
        { "<leader>l", group = "lsp" },
        { "<leader>g", group = "git" },
        { "<leader>x", group = "exec" },
      },
    })
  end
}
