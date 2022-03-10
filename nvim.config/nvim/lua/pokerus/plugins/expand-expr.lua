return {
  plug = function(use)
    use {
      "AllenDang/nvim-expand-expr",
      opt = true,
      cmd = { "Expand" },
      config = function()
        vim.cmd [[command! Expand lua require("expand_expr").expand()]]
      end,
    }
  end,
}
