return {
  "johmsalas/text-case.nvim",
  event = "VeryLazy",
  keys = { "ga" },
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
  end,
}
