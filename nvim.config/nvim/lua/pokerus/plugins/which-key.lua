return {
  "folke/which-key.nvim",
  config = function()
    local wk = require("which-key")

    wk.setup()

    wk.add({
      { -- Override verbose descriptions
        mode = "n",
        { "gx", desc = "Open file or link" },
      },
      { -- Mnemonics for my own groups
        mode = { "n", "v" },
        { "<leader>l", group = "lsp" },
        { "<leader>g", group = "git" },
        { "<leader>x", group = "exec" },
      },
    })
  end
}
