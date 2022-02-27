return {
  plug = function(use)
    use {
      "RRethy/vim-illuminate",
      config = function()
        vim.g.Illuminate_delay = 69
        vim.g.Illuminate_highlightUnderCursor = 0
        vim.g.Illuminate_ftblacklist = { "nerdtree" }
      end,
    }
  end,
}
