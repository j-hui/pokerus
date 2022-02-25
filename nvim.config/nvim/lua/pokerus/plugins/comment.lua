return {
  plug = function(use)
    use {
      "numToStr/Comment.nvim",
      config = function()
        require("Comment").setup {}
        require("pokerus").nmap {
          g = {
            c = "line-comment",
            b = "block-comment",
          }
        }
      end,
    }
  end,
}
