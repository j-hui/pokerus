local M = {
  "ibhagwan/fzf-lua",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    { "junegunn/fzf", build = "./install --bin" }
  },
  cmd = { "FzfLua" },
}

M.config = function()
  local fzf = require("fzf-lua")

  require("fzf-lua").setup({
    winopts = {
      height   = 0.69, -- window height
      width    = 1.0,  -- window width
      row      = 1.0,  -- window row position (0=top, 1=bottom)
      col      = 0.5,  -- window col position (0=left, 1=right)
      backdrop = 69,   -- opacity of backdrop outside of fzf window
    },
    keymap = {
      builtin = {
        false,                 -- do not inherit from defaults
        ["<Esc>"]    = "hide", -- hide fzf-lua, `:FzfLua resume` to continue
        ["<C-/>"]    = "toggle-help",
        ["<C-o>"]    = "toggle-fullscreen",
        ["<S-left>"] = "preview-reset",
        ["<S-down>"] = "preview-page-down",
        ["<S-up>"]   = "preview-page-up",
        ["<M-d>"]    = "preview-page-down",
        ["<M-u>"]    = "preview-page-up",
        ["<M-n>"]    = "preview-down",
        ["<M-p>"]    = "preview-up",
      },
      fzf = {
        false, -- do not inherit from defaults
        ["ctrl-z"] = "abort",
        ["ctrl-k"] = "kill-line",
        ["ctrl-d"] = "half-page-down",
        ["ctrl-u"] = "half-page-up",
        ["ctrl-a"] = "beginning-of-line",
        ["ctrl-e"] = "end-of-line",
        ["ctrl-t"] = "toggle",
        ["alt-t"]  = "toggle-all",
      },
    },
    actions = {
      files = {
        false,
        ["enter"]  = fzf.actions.file_edit,
        ["ctrl-x"] = fzf.actions.file_split,
        ["ctrl-s"] = fzf.actions.file_vsplit,
        ["ctrl-y"] = fzf.actions.file_sel_to_qf,
      },
    },
  })

  fzf.register_ui_select()
end

local function cmd(name)
  return function()
    require("fzf-lua")[name]()
  end
end

function M.init()
  require("pokerus.keybinds").add_prefix("<leader>/", "grep")
end

M.keys = {
  { "<leader><leader>", cmd("resume"),        desc = "fzf-resume" },

  { "<leader>h",        cmd("help_tags"),     desc = "help-tags" },
  { "<leader>H",        cmd("manpages"),      desc = "man-pages" },

  { "<leader>e",        cmd("files"),         desc = "files" },
  { "<leader>E",        cmd("oldfiles"),      desc = "old-files" },
  { "<leader>b",        cmd("buffers"),       desc = "buffers" },
  { "<leader>B",        cmd("lines"),         desc = "lines" },

  { "<leader>//",       cmd("grep"),          desc = "grep" },
  { "<leader>/.",       cmd("grep_cword"),    desc = "grep-cword" },
  { "<leader>/",        cmd("grep_visual"),   desc = "grep-visual",   mode = { "v" } },

  { "<C-g>x",           cmd("complete_path"), desc = "complete-path", mode = { "n", "v", "i" } },
  { "<C-g>p",           cmd("complete_line"), desc = "complete-line", mode = { "n", "v", "i" } },
}

return M
