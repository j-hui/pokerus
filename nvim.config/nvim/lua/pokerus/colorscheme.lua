local M = {}

M.colorschemes = {
  {
    "nyoom-engineering/oxocarbon.nvim",
    schemename = "oxocarbon",
    main = true,
  },
  {
    "folke/tokyonight.nvim",
    schemename = "tokyonight",
    opts = {
      style = "night",
      styles = {
        keywords = { italic = false }
      },
      sidebars = { "qf", "vista_kind", "terminal", "packer" }
    },
  },
  {
    "projekt0n/github-nvim-theme",
    modname = "github-theme",
    schemename = "github_dark",
    opts = {
      keyword_style = "bold",
    },
  },
}

for _, colo in ipairs(M.colorschemes) do
  if not colo.schemename then
    vim.notify("Error: did not specify colorscheme name for " .. colo[1], vim.log.levels.ERROR)
  else
    colo.lazy, colo.priority = true, 1000

    if colo.main then
      colo.lazy = false
      M.main = colo.schemename
    end

    colo.config = function(_, opts)
      if colo.opts then
        require(colo.modname or colo.schemename).setup(opts)
      end
      vim.cmd.colorscheme(colo.schemename)
    end
  end
end

if not M.main then
  vim.notify("Warning: no main theme specified. Defaulting to habamax.", vim.log.levels.WARN)
  M.main = "habamax"
  vim.cmd.colorscheme(M.main)
end

return M
