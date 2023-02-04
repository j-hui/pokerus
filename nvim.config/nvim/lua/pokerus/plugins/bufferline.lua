return {
  "akinsho/bufferline.nvim",
  dependencies = {
    "kyazdani42/nvim-web-devicons",
  },
  lazy = false,
  keys = {
    { "[b", "<cmd>BufferLineCyclePrev<CR>", desc = "buffer-prev" },
    { "]b", "<cmd>BufferLineCycleNext<CR>", desc = "buffer-next" },
    { "[B", "<cmd>BufferLineMovePrev<CR>", desc = "buffer-move-prev" },
    { "]B", "<cmd>BufferLineMoveNext<CR>", desc = "buffer-move-next" },
  },
  opts = {
    options = {
      diagnostics = "nvim_lsp",
      numbers = "buffer_id",
      ---@diagnostic disable-next-line: unused-local
      diagnostics_indicator = function(count, level, diagnostics_dict, context)
        if level:match "error" then
          return "  " .. count
        end
        return ""
      end,
    },
  },
}
