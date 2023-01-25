return {
  "kevinhwang91/nvim-ufo",
  event = "VeryLazy",
  dependencies = {
    "kevinhwang91/promise-async",
    "nvim-treesitter/nvim-treesitter",
  },
  config = function()
    vim.o.foldcolumn = '1' -- '0' is not bad
    vim.o.foldlevel = 99 -- Using ufo provider need a large value, feel free to decrease the value
    vim.o.foldlevelstart = 99
    vim.o.foldenable = true

    require("ufo").setup {
      provider_selector = function()
        return { 'treesitter', 'indent' }
      end
    }

    vim.keymap.set("n", "zR", require("ufo").openAllFolds, { desc = "open-all-folds" })
    vim.keymap.set("n", "zM", require("ufo").closeAllFolds, { desc = "close-all-folds" })
    vim.keymap.set("n", "zK", require("ufo").peekFoldedLinesUnderCursor, { desc = "peek-folds" })
  end,
}
