return {
  plug = function(use)
    use {
      "kosayoda/nvim-lightbulb",
      -- Make code actions more visible
      requires = { "neovim/nvim-lspconfig" },
      config = function()
        vim.cmd [[
          sign define LightBulbSign text=↯ texthl=SignColumn
          augroup NvimLightbulb
            autocmd!
            autocmd CursorHold,CursorHoldI * lua require'nvim-lightbulb'.update_lightbulb()
          augroup END
        ]]
      end,
    }
  end,
}
