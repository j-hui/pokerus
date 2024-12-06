return {
  "tpope/vim-fugitive",
  event = "VeryLazy",
  cmd = { "G", "Git" },
  keys = {
    -- { "<leader>gd", "<cmd>Gdiffsplit<CR>", mode = "n", desc = "git-diff-split" },
    -- { "<leader>gD", "<cmd>Git diff --cached<CR>", mode = "n", desc = "git-diff-cached" },
    { "<leader>gp", "<cmd>Git pull<CR>", mode = "n", desc = "git-pull" },
    { "<leader>gP", "<cmd>Git push<CR>", mode = "n", desc = "git-push" },
    { "<leader>gc", "<cmd>Git commit<CR>", mode = "n", desc = "git-commit" },
    { "<leader>gC", "<cmd>Git commit --amend<CR>", mode = "n", desc = "git-amend" },
    -- { "<leader>gw", "<cmd>Gwrite<CR>", mode = "n", desc = "git-write" },
    { "<leader>gs", "<cmd>Git<CR>", mode = "n", desc = "git-status" },
  },
  config = function()
    require("pokerus").vimsetup("fugitive")
  end,
}
