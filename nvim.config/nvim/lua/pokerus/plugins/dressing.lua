return {
  plug = function(use)
    use {
      "stevearc/dressing.nvim",
      requires = { }, -- TODO:
      config = function()
        require("dressing").setup {}
      end,
    }
  end,
}
