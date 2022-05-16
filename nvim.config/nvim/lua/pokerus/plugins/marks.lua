return {
  plug = function(use)
    use {
      "chentoast/marks.nvim",
      config = function()
        require("marks").setup {}
      end,
    }
  end,
}
