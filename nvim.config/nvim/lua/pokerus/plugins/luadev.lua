return {
  "bfredl/nvim-luadev",
  ft = "lua",
  keys = {
    -- TODO: open luadev buffer automatically
    -- TODO: binding to close luadev buffer
    -- TODO: evaluate text object based on treesitter nodes
    { "<leader>x<space>", "<cmd>Luadev<CR>", mode = "n", desc = "luadev-buffer" },
    { "<leader>xc", "<Plug>(Luadev-Run)", mode = "n", desc = "luadev-run" },
    { "<leader>xx", "<Plug>(Luadev-RunLine)", mode = "n", desc = "luadev-run-line" },
    { "<leader>xk", "<Plug>(Luadev-RunWord)", mode = "n", desc = "luadev-run-word" },
  },
}
