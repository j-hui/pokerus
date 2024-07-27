return {
  "akinsho/bufferline.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
  },
  lazy = false,
  keys = {
    { "[b", "<cmd>BufferLineCyclePrev<CR>", desc = "buffer-prev" },
    { "]b", "<cmd>BufferLineCycleNext<CR>", desc = "buffer-next" },
    { "[B", "<cmd>BufferLineMovePrev<CR>",  desc = "buffer-move-prev" },
    { "]B", "<cmd>BufferLineMoveNext<CR>",  desc = "buffer-move-next" },
  },
  opts = {
    options = {
      diagnostics = "nvim_lsp",
      numbers = "buffer_id",
      ---@diagnostic disable-next-line: unused-local
      diagnostics_indicator = function(count, level, diagnostics_dict, context)
        -- Notes:
        -- - `context.buffer:current()` checks whether this is the current buffer
        -- - `count` is the total number of diagnostics
        if level:match("error") then
          return "  " .. tostring(diagnostics_dict["error"])
        elseif level:match("warning") then
          return "  " .. tostring(diagnostics_dict["warning"])
        else
          return ""
        end
      end,
    },
  },
}
