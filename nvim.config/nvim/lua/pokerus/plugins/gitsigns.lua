local M = {}

function M.plug(use)
  use {
    "lewis6991/gitsigns.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
    },
    config = function()
      require("gitsigns").setup {
        on_attach = function()
          local gs = package.loaded.gitsigns
          local m = require("pokerus")

          m.nmap({
            name = "git",
            a = { gs.stage_hunk, "git-stage-hunk" },
            u = { gs.undo_stage_hunk, "git-unstage-hunk" },
            r = { gs.reset_hunk, "git-reset-hunk" },
            k = { gs.blame_line, "git-blame" },
            -- NOTE: supplants git-messenger

            x = { gs.toggle_current_line_blame, "git-show-blame" },
            z = { gs.toggle_deleted, "git-show-deleted" },
          }, { prefix = "<leader>g" })

          m.vmap({
            name = "git",
            a = { gs.stage_hunk, "git-stage-hunk" },
            r = { gs.reset_hunk, "git-reset-hunk" },
          }, { prefix = "<leader>g" })

          m.map {
            ["[g"] = { gs.prev_hunk, "git-prev-hunk" },
            ["]g"] = { gs.next_hunk, "git-next-hunk" },
          }

          m.omap {
            ["ih"] = { ":<C-U>Gitsigns select_hunk<CR>", "inner-git-hunk" },
          }

          m.xmap {
            ["ih"] = { ":<C-U>Gitsigns select_hunk<CR>", "inner-git-hunk" },
          }
        end,
      }
    end,
  }
end

return M
