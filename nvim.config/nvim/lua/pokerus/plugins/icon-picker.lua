return {
    "ziontee113/icon-picker.nvim",
    keys = {
      { "<leader>iy", "<cmd>IconPickerYank<cr>", mode = "n", desc = "icon-yank" },
      { "<leader>ii", "<cmd>IconPickerNormal<cr>", mode = "n", desc = "icon-insert" },
      { "<C-g>i", "<cmd>IconPickerInsert<cr>", mode = "i", desc = "icon-insert" },
    },
    config = function()
        require("icon-picker").setup({ disable_legacy_commands = true })
    end
}
