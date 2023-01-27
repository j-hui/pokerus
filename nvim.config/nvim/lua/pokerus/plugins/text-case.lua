return {
  "johmsalas/text-case.nvim",
  dependencies = { "nvim-telescope/telescope.nvim" },
  event = "VeryLazy",
  keys = {
    { mode = { "n", "v" }, "ga<leader>", "<cmd>TextCaseOpenTelescope<CR>", desc = "Telescope" },
    { mode = { "n", "v" }, "<leader>lR", "<cmd>TextCaseOpenTelescopeLSPChange<CR>", desc = "lsp-refactor-case" },
    { mode = { "n", "v" }, "ga;", "<cmd>TextCaseStartReplacingCommand<CR>", desc = "Start replacing" },
    { mode = { "n", "v" }, "ga:", "<cmd>TextCaseStartReplacingCommand<CR>", desc = "Start replacing" },
  },
  config = function()
    local tc, prefix = require("textcase"), "ga"

    tc.setup { prefix = prefix }

    -- These are missing from the default presets for some reason
    tc.register_keybindings(prefix, tc.api.to_snake_case, {
      prefix = prefix,
      quick_replace = "s",
      operator = "os",
      lsp_rename = "S",
    })

    tc.register_keybindings(prefix, tc.api.to_phrase_case, {
      prefix = prefix,
      quick_replace = ",",
      operator = "o,",
      lsp_rename = "<",
    })

    tc.register_keybindings(prefix, tc.api.to_dot_case, {
      prefix = prefix,
      quick_replace = ".",
      operator = "o.",
      lsp_rename = ">",
    })

    tc.register_keybindings(prefix, tc.api.to_path_case, {
      prefix = prefix,
      quick_replace = "/",
      operator = "o/",
      lsp_rename = "?",
    })

    require("telescope").load_extension("textcase")
  end,
}
