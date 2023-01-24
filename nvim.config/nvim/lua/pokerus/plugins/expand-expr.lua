return {
  "AllenDang/nvim-expand-expr",
  cmd = { "Expand" },
  config = function()
    vim.cmd [[command! Expand lua require("expand_expr").expand()]]
  end,
}
