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

local imaps = {
  ["<C-a>"] = { "<Home>", type = "command" },
  ["<C-e>"] = { "<End>", type = "command" },
  ["<C-f>"] = { "<Right>", type = "command" },
  ["<C-b>"] = { "<Left>", type = "command" },
  ["<C-h>"] = { "<BS>", type = "command" },
  ["<C-k>"] = function()
    vim.cmd "norm! d$"
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

function M.plug(use)
  use {
    "nvim-telescope/telescope.nvim",
    requires = {
      "nvim-lua/plenary.nvim",
      { "nvim-telescope/telescope-fzf-native.nvim", run = "make" },
      "nvim-telescope/telescope-file-browser.nvim",
    },
    config = function()
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
                ["<C-y>"] = fb_action "create",
                ["<C-t>"] = fb_action "toggle_browser",
              }),
            },
          },
        },
      }
      require("telescope").load_extension "fzf"
      require("telescope").load_extension "file_browser"

      local function builtin(name, opt)
        return function()
          require("telescope.builtin")[name](opt)
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

      require("pokerus").nmap({
        f = { builtin "find_files", "telescope-files" },
        F = {
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
        s = {
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
    end,
  }
end
return M
