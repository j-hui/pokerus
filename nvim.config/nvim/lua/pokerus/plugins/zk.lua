return {
  plug = function(use)
    use {
      "mickael-menu/zk-nvim",
      opt = true,
      event = "VimEnter",
      config = function()
        require("zk").setup {}
      end,
    }
  end,
}
