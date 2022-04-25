local M = {}

function M.config()
  vim.fn.sign_define(
    "LightBulbSign",
    { text = "↯", texthl = "SignColumn", linehl = "", numhl = "" }
  )

  require("nvim-lightbulb").setup {
    ignore = { "null-ls" },
  }

  vim.cmd [[
    augroup NvimLightbulb
      autocmd!
      autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
    augroup END
  ]]
end

function M.plug(use)
  use {
    "kosayoda/nvim-lightbulb",
    -- Make code actions more visible
    requires = { "neovim/nvim-lspconfig" },
    config = function()
      require("pokerus.plugins.lightbulb").config()
    end,
  }
end

return M
