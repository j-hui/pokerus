return {
  "carbon-steel/detour.nvim",
  config = function()
    vim.keymap.set('n', '<C-w><SPACE>', "<Cmd>Detour<CR>", { desc = "detour-open" })
  end
}
