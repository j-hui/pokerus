return {
  plug = function(use)
    use {
      "chentau/marks.nvim",
      config = function()
        require("marks").setup {}
      end,
    }
  end,
}
