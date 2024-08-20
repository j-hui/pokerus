local M = {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "keyvchan/telescope-find-pickers.nvim",
    "debugloop/telescope-undo.nvim",
  },
  cmd = { "Telescope" },
  extensions = {}
}

function M.builtin(name, opt)
  return function()
    require("telescope.builtin")[name](opt)
  end
end

function M.builtin_cwd(name, opt)
  opt = opt or {}
  return function()
    require("telescope.builtin")[name](
      vim.tbl_extend("keep", opt, {
        cwd = require("telescope.utils").buffer_dir()
      }))
  end
end

function M.extension(name, opt)
  return function()
    require("telescope").extensions[name][name](opt)
  end
end

M.keys = {
  { "<leader><leader>", M.extension "find_pickers",                                                  desc = "telescope" },
  { "<leader>e",        M.builtin("find_files", { no_ignore = true, hidden = true }),                desc = "telescope-files" },
  { "<leader>E",        M.builtin("find_files", { no_ignore = true, hidden = true, cwd = "%:p:h" }), desc = "telescope-files-here" },
  { "<leader>b",        M.builtin "buffers",                                                         desc = "telescope-buffers" },
  { "<leader>*",        M.builtin "grep_string",                                                     desc = "telescope-grep-word" },
  { "<leader>r",        M.builtin "live_grep",                                                       desc = "telescope-grep-live" },
  { "<leader>/",        M.builtin_cwd("live_grep", { grep_open_files = true }),                      desc = "telescope-grep-bufs" },
  { "<leader>h",        M.builtin "help_tags",                                                       desc = "telescope-help" },
  { "<leader>H",        M.builtin "man_pages",                                                       desc = "telescope-man-pages" },
  { "<leader>;",        M.builtin "commands",                                                        desc = "telescope-commands" },
  { "<leader>:",        M.builtin "commands",                                                        desc = "telescope-commands" },
  { "<leader>u",        M.extension "undo",                                                          desc = "telescope-undo" },

  { "<leader>gf",       M.builtin "git_files",                                                       desc = "git-files" },
  { "<leader>gF",       M.builtin "git_status",                                                      desc = "git-modified-files" },
  { "<leader>gl",       M.builtin "git_commits",                                                     desc = "git-log" },
  { "<leader>gL",       M.builtin "git_bcommits",                                                    desc = "git-buffer-log" },
  { "<leader>gb",       M.builtin "git_branches",                                                    desc = "git-branches" },

  { "<leader>ld",       M.builtin "diagnostics",                                                     desc = "telescope-diagnostics" },
  { "<leader>l?",       M.builtin "lsp_workspace_symbols",                                           desc = "lsp-workspace-symbols" },
  { "<leader>l/",       M.builtin "lsp_document_symbols",                                            desc = "lsp-document-symbols" },
}

function M.init()
  -- Override some of my own lsp handlers
  local lsp = require("pokerus.lsp")

  ---@diagnostic disable-next-line: duplicate-set-field
  lsp.definition = function(opt)
    opt = opt or {}
    require("telescope.builtin").lsp_definitions({ jump_type = opt.jump })
  end

  ---@diagnostic disable-next-line: duplicate-set-field
  lsp.implementation = function(opt)
    opt = opt or {}
    require("telescope.builtin").lsp_implementations({ jump_type = opt.jump })
  end

  ---@diagnostic disable-next-line: duplicate-set-field
  lsp.typedef = function(opt)
    opt = opt or {}
    require("telescope.builtin").lsp_type_definitions({ jump_type = opt.jump })
  end

  ---@diagnostic disable-next-line: duplicate-set-field
  lsp.references = function(opt)
    opt = opt or {}
    require("telescope.builtin").lsp_references({ jump_type = opt.jump })
  end
end

function M.config()
  local telescope = require "telescope"
  local action = require "telescope.actions"
  local function action_set(f, ...)
    local args = { ... }
    return function(b)
      require("telescope.actions.set")[f](b, unpack(args))
    end
  end

  local imaps = {
    ["<C-a>"] = { "<Home>", type = "command" },
    ["<C-e>"] = { "<End>", type = "command" },
    ["<C-f>"] = { "<Right>", type = "command" },
    ["<C-b>"] = { "<Left>", type = "command" },
    ["<C-h>"] = { "<BS>", type = "command" },
    ["<C-k>"] = { "<cmd>norm! d$<cr>", type = "command" },
    ["<Esc>"] = action.close,
    ["<C-]>"] = { "<Esc>", type = "command" },
    -- NOTE: scroll = half-page, shift = single entry
    ["<C-u>"] = action_set("scroll_results", -1),
    ["<C-d>"] = action_set("scroll_results", 1),
    ["<C-p>"] = action_set("shift_selection", -1),
    ["<C-n>"] = action_set("shift_selection", 1),
    ["<M-u>"] = action_set("scroll_previewer", -1),
    ["<M-d>"] = action_set("scroll_previewer", 1),
    ["<C-x>"] = action.which_key,
  }

  local nmaps = {
    ["<Up>"] = { "k", type = "command" },
    ["<Down>"] = { "j", type = "command" },
    ["<Left>"] = { "h", type = "command" },
    ["<Right>"] = { "l", type = "command" },
    ["q"] = action.close,
    ["<C-u>"] = action_set("scroll_results", -1),
    ["<C-d>"] = action_set("scroll_results", 1),
    ["<C-p>"] = action_set("shift_selection", -1),
    ["<C-n>"] = action_set("shift_selection", 1),
    ["<M-u>"] = action_set("scroll_previewer", -1),
    ["<M-d>"] = action_set("scroll_previewer", 1),
  }

  telescope.setup({
    defaults = require("telescope.themes").get_ivy({
      winblend = 20,
      mappings = { i = imaps, n = nmaps },
      vimgrep_arguments = {
        "rg",
        "--color=never",
        "--no-heading",
        "--with-filename",
        "--line-number",
        "--column",
        "--smart-case",
        "-u",
        "-u",
      },
    }),
    pickers = {
      find_files = {
        follow = true, -- follow symlinks
      },
    },
    extensions = M.extensions,
  })
  telescope.load_extension "fzf"

  local cmd = vim.api.nvim_create_user_command
  cmd("Highlights", ":Telescope highlights", { desc = "telescope-highlights" })

  cmd("Rg", function(t)
    require("telescope.builtin").grep_string { search = t.args }
  end, { desc = "telescope-grep", nargs = "+" })
end

return M
