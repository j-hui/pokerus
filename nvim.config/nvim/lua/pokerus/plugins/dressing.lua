return {
  plug = function(use)
    use {
      "stevearc/dressing.nvim",
      requires = { },
      config = function()
        require("dressing").setup {}
      end,
    }
  end,
}
