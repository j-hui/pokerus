local M = {}

function M.maps()
  require("pokerus").nmap({
    c = {
      name = "coqtail",
      Q = { "<Plug>CoqStart", "coq-start" },
      q = { "<Plug>CoqStop", "coq-stop" },
      D = { "<Plug>CoqToggleDebug", "coq-toggle-debug" },
      r = { "<Plug>CoqRestorePanels", "coq-restore-panels" },

      l = { "<Plug>CoqToLine", "coq-to-line" },
      c = { "mz$<Plug>CoqToLine`z", "coq-to-eol" },
      n = { "<Plug>CoqNext", "coq-next" },
      p = { "<Plug>CoqUndo", "coq-undo" },
      e = { "<Plug>CoqJumpToEnd", "coq-jump-to-end" },

      g = { "<Plug>CoqGotoGoalStart", "coq-goto-goal-start" },
      G = { "<Plug>CoqGotoGoalEnd", "coq-goto-goal-end" },

      d = { "<Plug>CoqGotoDef", "coq-goto-def" },

      j = { "<Plug>CoqCheck", "coq-check" },
      k = { "<Plug>CoqAbout", "coq-about" },
      h = { "<Plug>CoqPrint", "coq-print" },
      ["/"] = { "<Plug>CoqSearch", "coq-search" },
      ["="] = { "<Plug>CoqLocate", "coq-locate" },
    },
  }, { prefix = "<leader>", buffer = 0, noremap = false })

  require("pokerus").xmap({
    c = {
      name = "coqtail",
      j = { "<Plug>CoqCheck", "coq-check" },
      k = { "<Plug>CoqAbout", "coq-about" },
      h = { "<Plug>CoqPrint", "coq-print" },
      ["/"] = { "<Plug>CoqSearch", "coq-search" },
      ["="] = { "<Plug>CoqLocate", "coq-locate" },
    },
  }, { prefix = "<leader>", buffer = 0, noremap = false })

  require("pokerus").imap({
    l = { "<Esc><Plug>CoqToLine", "coq-to-line" },
    c = { "<Esc>mz$<Plug>CoqToLine`z", "coq-to-eol" },
  }, { prefix = "<C-c>", buffer = 0, noremap = false })

  require("pokerus").nmap {
    ["<C-]>"] = { "<Plug>CoqGotoDef" },

    ["]c"] = { "<Plug>CoqGotoGoalNextStart", "coq-goto-goal-next" },
    ["]C"] = { "<Plug>CoqGotoGoalNextEnd", "coq-goto-goal-next-end" },
    ["[c"] = { "<Plug>CoqGotoGoalPrevStart", "coq-goto-goal-prev" },
    ["[C"] = { "<Plug>CoqGotoGoalPrevEnd", "coq-goto-goal-prev-end" },
    ["[d"] = { "<Plug>CoqJumpToError", "coq-jump-to-error" },
    ["]d"] = { "<Plug>CoqJumpToError", "coq-jump-to-error" },
  }
end

function M.plug(use)
  use {
    "whonore/coqtail",
    config = function()
      vim.g.coqtail_match_shift = 1
      vim.g.coqtail_indent_on_dot = 1
      vim.g.coqtail_auto_set_proof_diffs = "on"
      vim.g.coqtail_update_tagstack = 1
      vim.g.coqtail_nomap = 1

      vim.cmd [[
        augroup coqtail_commands
          autocmd!
          autocmd ColorScheme *
                \   highlight def CoqtailChecked ctermbg=236
                \|  highlight def CoqtailSent    ctermbg=237
          autocmd Filetype coq,coq-infos,coq-goals lua require("coqtail").maps()
          autocmd Filetype coq syntax sync fromstart
        augroup END
      ]]
    end,
  }
end

return M
