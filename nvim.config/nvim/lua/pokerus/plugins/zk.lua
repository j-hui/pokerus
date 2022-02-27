return {
  plug = function(use)
    use {
      "mickael-menu/zk-nvim",
      config = function()
        require("zk").setup {}
      end,
    }
  end,
}
