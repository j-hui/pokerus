return {
  plug = function(use)
    use {
      "nacro90/numb.nvim",
      config = function()
        require("numb").setup {}
      end,
    }
  end,
}
