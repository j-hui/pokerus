return {
  "tamago324/lir.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "kyazdani42/nvim-web-devicons",
    "tamago324/lir-git-status.nvim",
  },
  config = function()
    local actions = require "lir.actions"
    local mark_actions = require "lir.mark.actions"
    local clipboard_actions = require "lir.clipboard.actions"

    require("lir").setup {
      mappings = {
        ["<CR>"] = actions.edit,
        ["<C-s>"] = actions.split,
        ["<C-v>"] = actions.vsplit,
        ["<C-t>"] = actions.tabedit,

        ["-"] = actions.up,
        ["q"] = actions.quit,

        ["O"] = actions.mkdir,
        ["o"] = actions.newfile,
        ["r"] = actions.rename,
        ["@"] = actions.cd,
        ["Y"] = actions.yank_path,
        ["."] = actions.toggle_show_hidden,
        ["D"] = actions.delete,

        ["J"] = function()
          mark_actions.toggle_mark()
          vim.cmd "normal! j"
        end,
        ["C"] = clipboard_actions.copy,
        ["X"] = clipboard_actions.cut,
        ["P"] = clipboard_actions.paste,
      },
    }
    require("lir.git_status").setup { show_ignored = false }
  end,
}
