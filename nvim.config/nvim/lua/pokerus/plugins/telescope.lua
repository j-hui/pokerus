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
      vim.tbl_extend("keep", opt, {
        cwd = require("telescope.utils").buffer_dir()
      }))
  end
end

local my_fb_action = {
  -- open up cwd using telescope find_files
  find_files = function(prompt_bufnr)
    local fb_utils = require("telescope._extensions.file_browser.utils")
    local action_state = require("telescope.actions.state")

    local selections = fb_utils.get_selected_files(prompt_bufnr, false)
    local search_dirs = vim.tbl_map(function(path) return path:absolute() end, selections)
    if vim.tbl_isempty(search_dirs) then
      local current_finder = action_state.get_current_picker(prompt_bufnr).finder
      search_dirs = { current_finder.path }
    end
    require("telescope.actions").close(prompt_bufnr)
    print("finding in " .. vim.inspect(search_dirs))
    require("telescope.builtin").find_files({ search_dirs = search_dirs })
  end,

  -- live_grep only within current path or multi-selected files
  live_grep = function(prompt_bufnr)
    local fb_utils = require("telescope._extensions.file_browser.utils")
    local action_state = require("telescope.actions.state")

    local selections = fb_utils.get_selected_files(prompt_bufnr, false)
    local search_dirs = vim.tbl_map(function(path) return path:absolute() end, selections)
    if vim.tbl_isempty(search_dirs) then
      local current_finder = action_state.get_current_picker(prompt_bufnr).finder
      search_dirs = { current_finder.path }
    end
    require("telescope.actions").close(prompt_bufnr)
    require("telescope.builtin").live_grep({ search_dirs = search_dirs })
  end,

  -- Toggle between cwd and current (non-telescope) buffer path
  toggle_path = function(prompt_bufnr)
    local fb_utils = require("telescope._extensions.file_browser.utils")
    local action_state = require("telescope.actions.state")
    local Path = require("plenary.path")

    local current_picker = action_state.get_current_picker(prompt_bufnr)
    local finder = current_picker.finder
    local bufr_path = Path:new(vim.fn.expand("#:p"))
    local bufr_parent_path = bufr_path:parent():absolute()

    if finder.path ~= bufr_parent_path then
      finder.path = bufr_parent_path
      fb_utils.selection_callback(current_picker, bufr_path:absolute())
    else
      finder.path = vim.loop.cwd()
    end
    fb_utils.redraw_border_title(current_picker)
    current_picker:refresh(finder, {
      new_prefix = fb_utils.relative_path_prefix(finder),
      reset_prompt = true,
      multi = current_picker._multi,
    })
  end,
}

-- For file browsing
local browse = {
  folders = extension("file_browser", { files = false }),
  folders_here = extension("file_browser", { files = false, path = "%:p:h" }),
  browser = extension("file_browser", { files = true }),
  browser_here = extension("file_browser", { files = true, path = "%:p:h" }),
  files = builtin("find_files", { no_ignore = true, hidden = true }),
  files_here = builtin("find_files", { no_ignore = true, hidden = true, cwd = "%:p:h" }),
}

return {
  "nvim-telescope/telescope.nvim",
  dependencies = {
    "nvim-lua/plenary.nvim",
    { "nvim-telescope/telescope-fzf-native.nvim", build = "make" },
    "nvim-telescope/telescope-file-browser.nvim",
    "keyvchan/telescope-find-pickers.nvim",
    "debugloop/telescope-undo.nvim",
  },
  cmd = "Telescope",
  keys = {
    { "<leader><leader>", extension "find_pickers",                             desc = "telescope" },
    { "<leader>f",        browse.folders,                                       desc = "telescope-folders" },
    { "<leader>F",        browse.folders_here,                                  desc = "telescope-folders-." },
    { "<leader>e",        browse.browser,                                       desc = "telescope-browser" },
    { "<leader>E",        browse.browser_here,                                  desc = "telescope-browser-." },
    { "<leader>o",        browse.files,                                         desc = "telescope-files" },
    { "<leader>O",        browse.files_here,                                    desc = "telescope-files-." },
    { "<leader>b",        builtin "buffers",                                    desc = "telescope-buffers" },
    { "<leader>*",        builtin "grep_string",                                desc = "telescope-grep-word" },
    { "<leader>r",        builtin_cwd "live_grep",                              desc = "telescope-grep-live" },
    { "<leader>/",        builtin_cwd("live_grep", { grep_open_files = true }), desc = "telescope-grep-buf" },
    { "<leader>h",        builtin "help_tags",                                  desc = "telescope-help" },
    { "<leader>c",        builtin "commands",                                   desc = "telescope-commands" },
    { "<leader>K",        builtin "man_pages",                                  desc = "telescope-man-pages" },
    { "<leader>u",        extension "undo",                                     desc = "telescope-undo" },
    { "<leader>gf",       builtin "git_files",                                  desc = "git-files" },
    { "<leader>gm",       builtin "git_status",                                 desc = "git-modified" },
    { "<leader>gl",       builtin "git_commits",                                desc = "git-log" },
    { "<leader>gL",       builtin "git_bcommits",                               desc = "git-buffer-log" },
    { "<leader>gb",       builtin "git_branches",                               desc = "git-branches" },
    { "<leader>ld",       builtin "diagnostics",                                desc = "telescope-diagnostics" },
    { "<leader>l?",       builtin "lsp_workspace_symbols",                      desc = "lsp-workspace-symbols" },
    { "<leader>l/",       builtin "lsp_document_symbols",                       desc = "lsp-document-symbols" },
  },

  init = function()
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

    local cmd = vim.api.nvim_create_user_command
    cmd("Hi", ":Telescope highlights", { desc = "telescope-highlights" })
    cmd("Highlights", ":Telescope highlights", { desc = "telescope-highlights" })

    cmd("Rg", function(t)
      require("telescope.builtin").grep_string { search = t.args }
    end, { desc = "telescope-grep", nargs = "+" })
  end,

  config = function()
    local telescope = require "telescope"
    local action = require "telescope.actions"
    local fb_action = telescope.extensions.file_browser.actions
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


    telescope.setup {
      defaults = require("telescope.themes").get_ivy {
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
      },
      pickers = {
        find_files = {
          follow = true, -- follow symlinks
        },
      },
      extensions = {
        file_browser = {
          hidden = true,      -- show hidden files
          grouped = true,     -- group folders together
          cwd_to_path = true, -- use path as cwd
          follow_symlinks = true,
          collapse_dirs = true,
          mappings = {
            n = nmaps,
            i = vim.tbl_extend("force", imaps, {
              ["<C-w>"] = { "<cmd>norm! db<cr>", type = "command" },
              ["<S-CR>"] = fb_action.create_from_prompt,
              ["<BS>"] = fb_action.backspace,

              ["<C-g>"] = false,

              ["<C-g>o"] = fb_action.create,
              ["<C-g><CR>"] = fb_action.create_from_prompt,
              ["<C-g>r"] = fb_action.rename,
              ["<C-g>x"] = fb_action.remove,
              ["<C-g>m"] = fb_action.move, -- move selected to here
              ["<C-g>y"] = fb_action.copy, -- copy selected to here

              ["<C-g><C-g>"] = fb_action.toggle_browser,
              ["<C-g>h"] = fb_action.toggle_hidden,
              ["<C-g>i"] = fb_action.toggle_respect_gitignore,

              ["<C-g>-"] = fb_action.goto_parent_dir,
              ["<C-g>="] = fb_action.goto_cwd,
              ["<C-g>."] = fb_action.change_cwd,
              ["<C-g>~"] = fb_action.goto_home_dir,
              ["<C-g>`"] = my_fb_action.toggle_path,

              ["<C-g>a"] = fb_action.select_all,
              ["<C-g>d"] = fb_action.sort_by_date,
              ["<C-g>s"] = fb_action.sort_by_size,

              ["<C-g>/"] = my_fb_action.live_grep,
              ["<C-g>f"] = my_fb_action.find_files,
            }),
          },
        },
        undo = {
          side_by_side = true,
        },
      },
    }
    telescope.load_extension "fzf"
    telescope.load_extension "file_browser"
    telescope.load_extension "undo"
  end,
}
