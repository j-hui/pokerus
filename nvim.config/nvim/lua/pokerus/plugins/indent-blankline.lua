return {
  "lukas-reineke/indent-blankline.nvim",
  main = "ibl",
  event = "VeryLazy",
  config = function()
    local function set_highlights()
      vim.api.nvim_set_hl(0, "IblIndent", { link = "LineNr" })
      vim.api.nvim_set_hl(0, "IblWhitespace", { link = "LineNr" })
      vim.api.nvim_set_hl(0, "IblScope", { link = "Comment" })
    end

    require("ibl").setup()
    set_highlights()

    -- reset highlight groups every time the colorscheme changes
    local hooks = require("ibl.hooks")
    hooks.register(hooks.type.HIGHLIGHT_SETUP, set_highlights)
  end,
}
