return {
  plug = function(use)
    use {
      "lukas-reineke/virt-column.nvim",
      after = { "theme" },
      config = function()
        require("virt-column").setup {}
        vim.cmd [[highlight! default link VirtColumn EndOfBuffer]]
      end,
    }
  end,
}
