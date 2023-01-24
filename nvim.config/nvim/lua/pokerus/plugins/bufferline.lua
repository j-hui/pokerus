return {
  "akinsho/bufferline.nvim",
  dependencies = {
    "kyazdani42/nvim-web-devicons",
  },
  config = function()
    require("bufferline").setup {
      options = {
        diagnostics = "nvim_lsp",
        numbers = "buffer_id",
        diagnostics_indicator = function(count, level) -- , diagnostics_dict, context)
          if level:match "error" then
            return "  " .. count
          end
          return ""
        end,
      },
    }
    vim.keymap.set("n", "[b",  "<cmd>BufferLineCyclePrev<CR>", { desc = "buffer-prev", silent = true })
    vim.keymap.set("n", "]b",  "<cmd>BufferLineCycleNext<CR>", { desc = "buffer-next", silent = true })
  end,
}
