return {
  "Danielhp95/tmpclone-nvim",
  depedencies = { "nvim-lua/plenary.nvim", "nvim-telescope/telescope.nvim" },
  opts = {
    datadir = vim.fn.expand("~") .. "/extern",
  },
}
