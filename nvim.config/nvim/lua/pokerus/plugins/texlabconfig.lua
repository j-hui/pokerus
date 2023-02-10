if vim.fn.executable "go" then
  vim.g.use_texlabconfig = true
end

return vim.g.use_texlabconfig and {
  "f3fora/nvim-texlabconfig",
  dependencies = {
    "neovim/nvim-lspconfig",
  },
  config = true,
  ft = { "tex", "bib" }, -- for lazy loading
  build = "go build -o ~/.local/bin/",
} or {}
