local M = {}

local function action(f, ...)
  local args = { ... }
  return function(b)
    require("telescope.actions")[f](b, unpack(args))
  end
end

local function action_set(f, ...)
  local args = { ... }
  return function(b)
    require("telescope.actions.set")[f](b, unpack(args))
  end
end

local function fb_action(f, ...)
  local args = { ... }
  return function(b)
    require("telescope").extensions.file_browser.actions[f](b, unpack(args))
  end
end

local function builtin(name, opt)
  return function()
    require("telescope.builtin")[name](opt)
  end
end

local function extension(name, opt)
  return function()
    require("telescope").extensions[name][name](opt)
  end
end

local function builtin_cwd(name, opt)
  opt = opt or {}
  return function()
    require("telescope.builtin")[name](
      vim.tbl_extend(
        "keep",
        opt,
        { cwd = require("telescope.utils").buffer_dir() }
      )
    )
  end
end

local imaps = {
  ["<C-a>"] = { "<Home>", type = "command" },
  ["<C-e>"] = { "<End>", type = "command" },
  ["<C-f>"] = { "<Right>", type = "command" },
  ["<C-b>"] = { "<Left>", type = "command" },
  ["<C-h>"] = { "<BS>", type = "command" },
  ["<C-k>"] = function()
    vim.cmd [[norm! d$]]
  end,
  ["<Esc>"] = action "close",
}
local nmaps = {
  ["<Up>"] = { "k", type = "command" },
  ["<Down>"] = { "j", type = "command" },
  ["<Left>"] = { "h", type = "command" },
  ["<Right>"] = { "l", type = "command" },
  ["q"] = action "close",
  ["<C-u>"] = action_set("shift_selection", -12),
  ["<C-d>"] = action_set("shift_selection", 12),
  ["<C-p>"] = action_set("shift_selection", -1),
  ["<C-n>"] = action_set("shift_selection", 1),
}

function M.config()
  require("telescope").setup {
    defaults = require("telescope.themes").get_ivy {
      winblend = 20,
      mappings = { i = imaps, n = nmaps },
      file_ignore_patterns = { "node_modules", ".git" },
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
    },
    pickers = {
      find_files = {
        hidden = true,
      },
    },
    extensions = {
      file_browser = {
        mappings = {
          n = nmaps,
          i = vim.tbl_extend("force", imaps, {
            ["<C-o>"] = fb_action "create",
            ["<C-t>"] = fb_action "toggle_hidden",
            ["<C-]>"] = fb_action "goto_cwd",
            ["<C-l>"] = fb_action "change_cwd",
            ["~"] = fb_action "goto_home_dir",
            ["<C-w>"] = function()
              vim.cmd [[norm! db]]
            end,
          }),
        },
      },
    },
  }
  require("telescope").load_extension "fzf"
  require("telescope").load_extension "file_browser"

  require("pokerus").nmap({
    f = { extension "file_browser", "telescope-file-browser" },
    o = { builtin "find_files", "telescope-files" },
    O = {
      builtin("find_files", { no_ignore = true }),
      "telescope-files*",
    },
    e = { builtin_cwd "find_files", "telescope-edit" },
    E = {
      builtin_cwd("find_files", { no_ignore = true }),
      "telescope-edit*",
    },
    b = { builtin "buffers", "telescope-buffers" },
    ["*"] = { builtin "grep_string", "telescope-grep-word" },
    r = { builtin_cwd "live_grep", "telescope-grep-live" },
    ["/"] = {
      builtin_cwd("live_grep", { grep_open_files = true }),
      "telescope-sneak",
    },
    h = { builtin "help_tags", "telescope-help" },
    c = { builtin "commands", "telescope-commands" },
    K = { builtin "man_pages", "telescope-man-pages" },
  }, { prefix = "<leader>" })

  require("pokerus").nmap({
    name = "git",
    f = { builtin "git_files", "git-files" },
    m = { builtin "git_status", "git-modified" },
    l = { builtin "git_commits", "git-log" },
    L = { builtin "git_bcommits", "git-buffer-log" },
    b = { builtin "git_branches", "git-branches" },
  }, { prefix = "<leader>g" })

  require("pokerus").nmap({
    name = "git",
    f = { builtin "git_files", "git-files" },
    m = { builtin "git_status", "git-modified" },
    l = { builtin "git_commits", "git-log" },
    L = { builtin "git_bcommits", "git-buffer-log" },
    b = { builtin "git_branches", "git-branches" },
  }, { prefix = "<leader>g" })

  require("pokerus").nmap({
    name = "lsp",
    ["/"] = { builtin "lsp_workspace_symbols", "lsp-workspace-symbols" },
    s = { builtin "lsp_document_symbols", "lsp-document-symbols" },
  }, { prefix = "<leader>l" })

  vim.cmd [[
    command! H                  :Telescope help_tags
    command! Help               :Telescope help_tags

    command! Hi                 :Telescope highlights
    command! Highlights         :Telescope highlights

    command! M                  :Telescope man_pages
    command! Manpages           :Telescope man_pages

    command! -nargs=+ -bang Rg  :lua require'telescope.builtin'.grep_string{search=<q-args>}
    ]]
end

function M.plug(use)
  use {
    "nvim-telescope/telescope.nvim",
    opt = true,
    event = "VimEnter",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = [[require("pokerus.plugins.telescope").config()]],
  }
end
return M
