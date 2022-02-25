return {
  plug = function(use)
    use {
      "luukvbaal/stabilize.nvim",
      config = function()
        require("stabilize").setup {}
      end,
    }
  end,
}
