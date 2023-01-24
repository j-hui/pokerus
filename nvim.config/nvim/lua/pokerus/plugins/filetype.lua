vim.g.did_load_filetypes = 1
return {
  "nathom/filetype.nvim",
  config = function()
    require("filetype").setup {
      overrides = {
        extensions = {
          h = "c",
          v = "coq",
          x = "alex",
          y = "happy",
        },
      },
    }
  end,
}
