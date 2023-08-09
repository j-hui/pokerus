return {
  "tamago324/lir.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-tree/nvim-web-devicons",
    "tamago324/lir-git-status.nvim",
  },
  event = "VeryLazy",
  config = function()
    local actions = require "lir.actions"
    local mark_actions = require "lir.mark.actions"
    local clipboard_actions = require "lir.clipboard.actions"

    require("lir").setup {
      show_hidden_files = true,
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

    vim.api.nvim_create_autocmd("FileType", {
      pattern = "lir",
      callback = function()
        vim.api.nvim_buf_set_option(0, "buflisted", true)
        vim.api.nvim_buf_set_option(0, "bufhidden", "hide")
      end,
    })
  end,
}
