return {
  plug = function(use)
    use {
      "numToStr/Comment.nvim",
      config = require("Comment").setup,
    }
  end,
}
