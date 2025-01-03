local M = {
  "nvim-orgmode/orgmode",
  tag = "0.3.4",
  event = "VeryLazy",
  ft = { "org", "orgagenda" },
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "hrsh7th/nvim-cmp",
    { "chipsenkbeil/org-roam.nvim", tag = "0.1.0" },
  },
}

function M.org_path(file)
  return "~/Documents/org/" .. file
end

local function templates(target)
  return {
    t = {
      description = "task (w/o deadline)",
      headline = "Tasks",
      target = target,
      template = [[
** TODO %?
]],
    },
    e = {
      description = "event",
      headline = "Events",
      target = target,
      template = [[
** %?
   %^{start time}T
]],
    },
    d = {
      -- Dates that are important
      description = "date",
      headline = "Events",
      target = target,
      template = [[
** %?
   %^{start time}t
]],
    },
    n = {
      -- Stuff I learned that I want to remember; might be refiled to roam
      description = "note",
      headline = "Notes",
      target = target,
      template = [[
** %?
   %u
]],
    },
    i = {
      -- Stuff that I thought of; might be refiled to tasks
      description = "idea",
      headline = "Ideas",
      target = target,
      template = [[
** %?
   %u
]],
    },
  }
end

function M.config()
  require("orgmode").setup({
    org_agenda_files = M.org_path "*",
    org_default_notes_file = M.org_path "inbox.org",
    org_todo_keywords = {
      "TODO(t)",      -- To be started
      "PROGRESS(p)",  -- Started, but not complete
      "WAITING(w)",   -- Waiting on time or event
      "MAYBE(m)",     -- Waiting on some more thinking
      "|",
      "DONE(d)",      -- Completed or decided
      "DELEGATED(e)", -- Someone else will finish it
      "WONTDO(w)",    -- Abandoned or given up
    },
    org_capture_templates = {
      w = { description = "Work", subtemplates = templates(M.org_path "work.org") },
      p = { description = "Play", subtemplates = templates(M.org_path "play.org") },
      l = { description = "Life", subtemplates = templates(M.org_path "life.org") },
    },
    mappings = {
      capture = {
        org_capture_finalize = false,
        org_capture_refile = false,
      },
      org = {
        -- org_return = false,
        org_timestamp_up = "+",
        org_timestamp_down = "-",
        org_export = false,
        -- org_refile = false, -- Overridden by telescope-orgmode, below
        org_insert_link = false,
      },
    },
  })

  require("org-roam").setup({
    directory = M.org_path "roam",
    org_files = { M.org_path "*" },
  })

  -- Buffer-local options
  vim.api.nvim_create_autocmd("FileType", {
    pattern = "org",
    callback = function()
      vim.cmd [[set formatoptions-=r]]
      vim.cmd [[set tabstop=2]]
      -- vim.keymap.set('n', '<leader>or', require('telescope').extensions.orgmode.refile_heading, {
      --   desc = "org refile",
      --   buffer = e.buf,
      -- })
      -- vim.keymap.set('n', '<leader>oli', require('telescope').extensions.orgmode.insert_link, {
      --   desc = "org insert link",
      --   buffer = e.buf,
      -- })
    end,
  })
end

return M
