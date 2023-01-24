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

return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "nvim-telescope/telescope-file-browser.nvim",
    "keyvchan/telescope-find-pickers.nvim",
  },
  cmd = "Telescope",
  init = function()
    local function nmap(...)
      vim.keymap.set("n", ...)
    end

    nmap("<leader><leader>", extension "find_pickers", { desc = "telescope" })

    nmap("<leader>f", extension "file_browser", { desc = "telescope-file-browser" })
    nmap("<leader>o", builtin "find_files", { desc = "telescope-files" })
    nmap("<leader>O", builtin("find_files", { no_ignore = true }), { desc = "telescope-files*" })
    nmap("<leader>e", builtin_cwd "find_files", { desc = "telescope-cwd-files" })
    nmap("<leader>E", builtin_cwd("find_files", { no_ignore = true }), { desc = "telescope-cwd-files*" })
    nmap("<leader>b", builtin "buffers", { desc = "telescope-buffers" })
    nmap("<leader>*", builtin "grep_string", { desc = "telescope-grep-word" })
    nmap("<leader>r", builtin_cwd "live_grep", { desc = "telescope-grep-live" })
    nmap("<leader>/", builtin_cwd("live_grep", { grep_open_files = true }), { desc = "telescope-sneak" })
    nmap("<leader>h", builtin "help_tags", { desc = "telescope-help" })
    nmap("<leader>c", builtin "commands", { desc = "telescope-commands" })
    nmap("<leader>K", builtin "man_pages", { desc = "telescope-man-pages" })

    nmap("<leader>gf", builtin "git_files", { desc = "git-files" })
    nmap("<leader>gm", builtin "git_status", { desc = "git-modified" })
    nmap("<leader>gl", builtin "git_commits", { desc = "git-log" })
    nmap("<leader>gL", builtin "git_bcommits", { desc = "git-buffer-log" })
    nmap("<leader>gb", builtin "git_branches", { desc = "git-branches" })

    nmap("<leader>l/", builtin "lsp_workspace_symbols", { desc = "lsp-workspace-symbols" })
    nmap("<leader>ls", builtin "lsp_document_symbols", { desc = "lsp-document-symbols" })

    local cmd = vim.api.nvim_create_user_command
    cmd("H", ":Telescope help_tags", { desc = "telescope-help" })
    cmd("Help", ":Telescope help_tags", { desc = "telescope-help" })
    cmd("Hi", ":Telescope help_tags", { desc = "telescope-highlights" })
    cmd("Highlights", ":Telescope help_tags", { desc = "telescope-highlights" })
    cmd("M", ":Telescope man_pages", { desc = "telescope-man-pages" })
    cmd("Man", ":Telescope man_pages", { desc = "telescope-man-pages" })

    cmd("Rg", function(t)
      require("telescope.builtin").grep_string { search = t.args }
    end, { desc = "telescope-grep", nargs = "+" })

  end,
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
              ["<C-o>"] = fb_action "create",
              ["<C-g>"] = fb_action "toggle_hidden",
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
  end,
}
