local function gitsigns_maps()
  local gs = package.loaded.gitsigns

  vim.keymap.set("n", "<leader>ga", gs.stage_hunk, { desc = "git-stage-hunk" })
  vim.keymap.set("n", "<leader>gu", gs.undo_stage_hunk, { desc = "git-unstage-hunk" })
  vim.keymap.set("n", "<leader>gr", gs.reset_hunk, { desc = "git-reset-hunk" })
  vim.keymap.set("n", "<leader>gk", gs.blame_line, { desc = "git-blame" })

  vim.keymap.set("n", "<leader>gx", gs.toggle_current_line_blame, { desc = "git-show-blame" })
  vim.keymap.set("n", "<leader>gz", gs.toggle_deleted, { desc = "git-show-deleted" })

  vim.keymap.set("x", "<leader>ga", gs.stage_hunk, { desc = "git-stage-hunk" })
  vim.keymap.set("x", "<leader>gr", gs.reset_hunk, { desc = "git-reset-hunk" })

  vim.keymap.set("n", "[g", gs.prev_hunk, { desc = "git-prev-hunk" })
  vim.keymap.set("n", "]g", gs.next_hunk, { desc = "git-next-hunk" })
  vim.keymap.set("x", "[g", gs.prev_hunk, { desc = "git-prev-hunk" })
  vim.keymap.set("x", "]g", gs.next_hunk, { desc = "git-next-hunk" })

  vim.keymap.set("o", "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "inner-git-hunk" })
  vim.keymap.set("x", "ih", ":<C-U>Gitsigns select_hunk<CR>", { desc = "inner-git-hunk" })
end

return {
  "lewis6991/gitsigns.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  event = "VeryLazy",
  opts = {
    on_attach = gitsigns_maps,
  }
}
