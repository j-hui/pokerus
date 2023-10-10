return {
  "andymass/vim-matchup",
  -- Navigate around matching delimiters using % and others
  init = function()
    -- vim.g.matchup_matchparen_offscreen = { method = "popup" } -- this seems to be buggy
    vim.g.matchup_override_vimtex = 1
    vim.g.matchup_matchparen_deferred = 1
    vim.g.matchup_matchparen_hi_surround_always = 1 -- Show surrounding delimiters
    vim.g.matchup_matchparen_deferred_show_delay = 100
  end
}
