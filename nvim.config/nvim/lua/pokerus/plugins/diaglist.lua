return {
  "onsails/diaglist.nvim",
  event = "LspAttach",
  keys = {
    { "<leader>l.", function() require("diaglist").open_all_diagnostics() end, desc = "lsp-diagnostics" },
    { "<leader>lJ", function() require("diaglist").open_buffer_diagnostics() end, desc = "lsp-diagnostics" },
  },
  config = function()
    require("diaglist").init { debounce_ms = 150 }
  end
}
