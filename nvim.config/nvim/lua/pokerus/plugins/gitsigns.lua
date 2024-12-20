local function gs(name)
  return function()
    require("gitsigns")[name]()
  end
end

local function gsv(name)
  return function()
    require("gitsigns")[name] { vim.fn.line("."), vim.fn.line("v") }
  end
end

local function nav_hunk(fallback, direction, opts)
  return function()
    if vim.wo.diff then
      vim.cmd.normal { fallback, bang = true }
    else
      require("gitsigns").nav_hunk(direction, opts)
    end
  end
end

return {
  "lewis6991/gitsigns.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  event = "VeryLazy",
  init = function()
    require("pokerus.keybinds").add_prefix("<leader>g", "git")
  end,
  keys = {
    { desc = "git-stage-hunk",      "<leader>ga", gs "stage_hunk" },
    { desc = "git-stage-hunk",      "<leader>ga", gsv "stage_hunk",                             mode = "v" },
    { desc = "git-reset-hunk",      "<leader>gr", gs "reset_hunk" },
    { desc = "git-reset-hunk",      "<leader>gr", gsv "reset_hunk",                             mode = "v" },
    { desc = "git-undo-stage-hunk", "<leader>gu", gs "undo_stage_hunk" },

    { desc = "git-stage-buffer",    "<leader>gw", gs "stage_buffer" },
    { desc = "git-stage-buffer",    "<leader>gA", gs "stage_buffer" },
    { desc = "git-reset-buffer",    "<leader>gR", gs "reset_buffer" },

    { desc = "git-blame-line",      "<leader>gk", gs "blame_line" },
    { desc = "git-blame",           "<leader>gK", gs "blame" },
    { desc = "git-show-deleted",    "<leader>gx", gs "toggle_deleted" },

    { desc = "git-diff",            "<leader>gd", gs "diffthis" },

    { desc = "git-diff-next",       "]c",         nav_hunk("]c", "next") },
    { desc = "git-diff-prev",       "]c",         nav_hunk("[c", "prev") },

    { desc = "git-staged-next",     "]c",         nav_hunk("]c", "next", { target = "staged" }) },
    { desc = "git-staged-prev",     "]c",         nav_hunk("[c", "prev", { target = "staged" }) },

    { desc = "inner-git-hunk",      "ih",         ":<C-U>Gitsigns select_hunk<CR>",             mode = { "o", "x" } },
  },
  config = function()
    require("gitsigns").setup()

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "gitsigns-blame",
      callback = function()
        vim.keymap.set("n", "q", "<cmd>quit<CR>", { buffer = true })
      end,
    })
  end,
}
