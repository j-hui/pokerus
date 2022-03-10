return {
  plug = function(use)
    use {
      "booperlv/nvim-gomove",
      config = function()
        require("gomove").setup {}
      end,
    }
  end,
}
