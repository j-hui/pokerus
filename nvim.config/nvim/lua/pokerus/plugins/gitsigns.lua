local function gitsigns_attach(bufnr)
  local gs = require("gitsigns")

  local function map(desc, mode, l, r, opts)
    opts = opts or {}
    opts.desc = desc
    opts.buffer = bufnr
    vim.keymap.set(mode, l, r, opts)
  end

  local function when_diff_do(keys)
    if vim.wo.diff then
      vim.cmd.normal({ keys, bang = true })
      return true
    else
      return false
    end
  end

  map("git-hunk-next", "n", "]c", function() _ = when_diff_do("]c") or gs.nav_hunk("next") end)
  map("git-hunk-prev", "n", "[c", function() _ = when_diff_do("[c") or gs.nav_hunk("prev") end)
  map("git-staged-next", "n", "]C", function() _ = when_diff_do("]c") or gs.nav_hunk("next", { target = "staged" }) end)
  map("git-staged-next", "n", "[C", function() _ = when_diff_do("[c") or gs.nav_hunk("prev", { target = "staged" }) end)

  map("inner-git-hunk", { "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
  map("git-stage-hunk", "n", "<leader>ga", gs.stage_hunk)
  map("git-reset-hunk", "n", "<leader>gr", gs.reset_hunk)
  map("git-stage-hunk", "v", "<leader>ga", function() gs.stage_hunk { vim.fn.line("."), vim.fn.line("v") } end)
  map("git-reset-hunk", "v", "<leader>gr", function() gs.reset_hunk { vim.fn.line("."), vim.fn.line("v") } end)
  map("git-undo-stage-hunk", "n", "<leader>hu", gs.undo_stage_hunk)

  map("git-stage-buffer", "n", "<leader>gw", gs.stage_buffer)
  map("git-stage-buffer", "n", "<leader>gA", gs.stage_buffer)
  map("git-reset-buffer", "n", "<leader>gR", gs.reset_buffer)

  map("git-blame-line", "n", "<leader>gk", gs.blame_line)
  map("git-blame", "n", "<leader>gK", gs.blame)
  map("git-show-deleted", "n", "<leader>gx", gs.toggle_deleted)

  map("git-diff", "n", "<leader>gd", gs.diffthis)
end

return {
  "lewis6991/gitsigns.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  event = "VeryLazy",
  config = function()
    require("gitsigns").setup({
      on_attach = gitsigns_attach,
    })

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "gitsigns-blame",
      callback = function()
        vim.keymap.set("n", "q", "<cmd>quit<CR>", { buffer = true })
      end,
    })
  end,
}
