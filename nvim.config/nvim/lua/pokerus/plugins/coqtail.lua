vim.g.coqtail_match_shift = 1
vim.g.coqtail_indent_on_dot = 1
vim.g.coqtail_auto_set_proof_diffs = "on"
vim.g.coqtail_update_tagstack = 1
vim.g.coqtail_nomap = 1

local function def_coqtail_maps(t)
  local function nmap(l, r, d)
    vim.keymap.set("n", l, r, { desc = d, buffer = t.buf, silent = true })
  end

  local function xmap(l, r, d)
    vim.keymap.set("x", l, r, { desc = d, buffer = t.buf })
  end

  local function imap(l, r, d)
    vim.keymap.set("i", l, r, { desc = d, buffer = t.buf })
  end

  nmap("<leader>lQ", "<Plug>CoqStart", "coq-start")
  nmap("<leader>lq", "<Plug>CoqStop", "coq-stop")
  nmap("<leader>lD", "<Plug>CoqToggleDebug", "coq-toggle-debug")
  nmap("<leader>lr", "<Plug>CoqRestorePanels", "coq-restore-panels")

  nmap("<leader>ll", "<Plug>CoqToLine", "coq-to-line")
  nmap("<leader>lc", "mz$<Plug>CoqToLine`z", "coq-to-eol")
  nmap("<leader>ln", "<Plug>CoqNext", "coq-next")
  nmap("<leader>lp", "<Plug>CoqUndo", "coq-undo")
  nmap("<leader>le", "<Plug>CoqJumpToEnd", "coq-jump-to-end")

  nmap("<leader>lg", "<Plug>CoqGotoGoalStart", "coq-goto-goal-start")
  nmap("<leader>lG", "<Plug>CoqGotoGoalEnd", "coq-goto-goal-end")

  nmap("<leader>ld", "<Plug>CoqGotoDef", "coq-goto-def")

  nmap("<leader>lj", "<Plug>CoqCheck", "coq-check")
  nmap("<leader>lk", "<Plug>CoqAbout", "coq-about")
  nmap("<leader>lh", "<Plug>CoqPrint", "coq-print")
  nmap("<leader>l/", "<Plug>CoqSearch", "coq-search")
  nmap("<leader>l=", "<Plug>CoqLocate", "coq-locate")

  xmap("<leader>lj", "<Plug>CoqCheck", "coq-check")
  xmap("<leader>lk", "<Plug>CoqAbout", "coq-about")
  xmap("<leader>lh", "<Plug>CoqPrint", "coq-print")
  xmap("<leader>l/", "<Plug>CoqSearch", "coq-search")
  xmap("<leader>l=", "<Plug>CoqLocate", "coq-locate")

  imap("<C-c>n", "<Esc><Plug>CoqNext", "coq-next")
  imap("<C-c>p", "<Esc><Plug>CoqUndo", "coq-undo")
  imap("<C-c>l", "<Esc><Plug>CoqToLine", "coq-to-line")
  imap("<C-c>c", "<Esc>mz$<Plug>CoqToLine`z", "coq-to-eol")

  nmap("<C-c>n", "<Plug>CoqNext", "coq-next")
  nmap("<C-c>p", "<Plug>CoqUndo", "coq-undo")
  nmap("<C-c>l", "<Plug>CoqToLine", "coq-to-line")
  nmap("<C-c>c", "mz$<Plug>CoqToLine`z", "coq-to-eol")
  nmap("<C-c><CR>", "mz$<Plug>CoqToLine`z", "coq-to-eol")

  nmap("<C-]>", "<Plug>CoqGotoDef", "coq-goto-def")

  nmap("]c", "<Plug>CoqGotoGoalNextStart", "coq-goto-goal-next")
  nmap("]C", "<Plug>CoqGotoGoalNextEnd", "coq-goto-goal-next-end")
  nmap("[c", "<Plug>CoqGotoGoalPrevStart", "coq-goto-goal-prev")
  nmap("[C", "<Plug>CoqGotoGoalPrevEnd", "coq-goto-goal-prev-end")
  nmap("[d", "<Plug>CoqJumpToError", "coq-jump-to-error")
  nmap("]d", "<Plug>CoqJumpToError", "coq-jump-to-error")
end

local function def_coqtail_highlights()
  local checked_hl = vim.api.nvim_get_hl_by_name("DiffChange")
  local sent_hl = vim.api.nvim_get_hl_by_name("DiffAdd")
  vim.api.nvim_set_hl(0, "CoqtailChecked", checked_hl)
  vim.api.nvim_set_hl(0, "CoqtailSent", sent_hl)
end

return {
  "whonore/coqtail",
  ft = "coq",
  config = function()
    def_coqtail_highlights()
    vim.api.nvim_create_autocmd("ColorScheme", { pattern = "*", callback = def_coqtail_highlights })
    vim.api.nvim_create_autocmd("Filetype",
      { pattern = { "coq", "coq-infos", "coq-goals" }, callback = def_coqtail_maps })
  end,
}
