return {
  "numToStr/Comment.nvim",
  keys = {
    { "gc", mode = "n", desc = "line-comment" },
    { "gb", mode = "n", desc = "block-comment" },
    { "gc", mode = "x", desc = "line-comment" },
    { "gb", mode = "x", desc = "block-comment" },
  },
  config = function()
    require("Comment").setup({})
    local ft = require('Comment.ft')
    ft({ 'alex', 'happy' }, { '--%s', '{-%s-}' })
  end,
}
