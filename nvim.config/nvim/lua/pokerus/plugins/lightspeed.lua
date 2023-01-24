vim.g.lightspeed_no_default_keymaps = true

return {
  "ggandor/lightspeed.nvim",
  event = "VeryLazy",
  opts = {
    exit_after_idle_msecs = { labeled = 2500, unlabled = 1000 },
    limit_ft_matches = 16,
  },
  keys = {
    { "f", "<Plug>Lightspeed_f", mode = "n", desc = "lightspeed-f" },
    { "F", "<Plug>Lightspeed_F", mode = "n", desc = "lightspeed-F" },
    { "t", "<Plug>Lightspeed_t", mode = "n", desc = "lightspeed-t" },
    { "T", "<Plug>Lightspeed_T", mode = "n", desc = "lightspeed-T" },
    { "s", "<Plug>Lightspeed_s", mode = "n", desc = "lightspeed-s" },
    { "S", "<Plug>Lightspeed_S", mode = "n", desc = "lightspeed-S" },
    { "g/", "<Plug>Lightspeed_omni_s", mode = "n", desc = "lightspeed-omni-s" },
    { ",", "<Plug>Lightspeed_;_ft", mode = "n", desc = "lightspeed-next" },
    { "[,", "<Plug>Lightspeed_;_ft", mode = "n", desc = "lightspeed-prev" },
    { "],", "<Plug>Lightspeed_,_ft", mode = "n", desc = "lightspeed-next" },
    { "c", "<Plug>Lightspeed_s", mode = "x", desc = "lightspeed-s" },
    { "C", "<Plug>Lightspeed_S", mode = "x", desc = "lightspeed-S" },
    { "c", "<Plug>Lightspeed_s", mode = "o", desc = "lightspeed-s" },
    { "C", "<Plug>Lightspeed_S", mode = "o", desc = "lightspeed-S" },
    { "x", "<Plug>Lightspeed_x", mode = "o", desc = "lightspeed-x" },
    { "X", "<Plug>Lightspeed_X", mode = "o", desc = "lightspeed-X" },
  },
}
