return {
  "andymass/vim-matchup",
  -- Navigate around matching delimiters using % and others
  init = function()
    -- vim.g.matchup_matchparen_offscreen = { method = "popup" } -- this seems to be buggy
    vim.g.matchup_override_vimtex = 1

    vim.g.matchup_matchparen_deferred = 1 -- keystrokes get laggy without these aggro settings
    vim.g.matchup_matchparen_deferred_show_delay = 500
    vim.g.matchup_matchparen_deferred_hide_delay = 1000
    vim.g.matchup_matchparen_deferred_fade_time = 1000

    vim.g.matchup_matchparen_hi_surround_always = 1 -- Show surrounding delimiters, requires deferred highlighting

    vim.g.matchup_surround_enabled = 1              -- cs% and ds%

    require("pokerus.plugins.treesitter").opts.endwise = { enable = true }
  end
}
