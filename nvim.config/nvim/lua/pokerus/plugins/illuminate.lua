return {
  "RRethy/vim-illuminate",
  -- known alternatives (that I've used before):
  -- - tzachar/local-highlight.nvim (regex-only)
  -- - nvim-treesitter/nvim-treesitter-refactor (ts-only)
  event = "VeryLazy",
  config = function()
    require("illuminate").configure {
      under_cursor = false,
      filetypes_denylist = {
        'dirbuf',
        'dirvish',
        'fugitive',
        'nerdtree',
        'tex',
        'bib',
      },
      large_file_cutoff = 10000,
      large_file_overrides = nil or {
        -- TODO: experiment with these
        providers = {
          'lsp',
          'treesitter',
          'regex',
        },
      },
    }
  end,
}
