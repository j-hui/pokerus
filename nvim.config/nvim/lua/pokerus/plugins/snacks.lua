local M = {
  "folke/snacks.nvim",
  priority = 1000,
  lazy = false,
}

M.opts = {}

M.opts.bigfile = {
  enabled = true,
  -- Enable or disable features when big file detected
  ---@param ctx {buf: number, ft: string}
  setup = function(ctx)
    vim.cmd([[NoMatchParen]])
    require("snacks").util.wo(0, { foldmethod = "manual", statuscolumn = "", conceallevel = 0 })
    vim.b.minianimate_disable = true
    vim.schedule(function()
      vim.bo[ctx.buf].syntax = ctx.ft
    end)
  end,
}

local rat = [[
        ██                            ████████████████
      ██▓▓██                ████  ████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██
    ██░░██▓▓██            ██▓▓▓▓██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓██
    ██░░██▓▓██████████  ██▓▓████░░██▓▓▓▓▓▓        ▓▓▓▓▓▓▓▓██
    ██░░░░██▓▓▓▓▓▓▓▓▓▓██▓▓██░░░░░░██▓▓▓▓              ▓▓▓▓▓▓██
    ██░░██▓▓▓▓▓▓▓▓▓▓██▓▓██░░░░░░░░██▓▓                  ▓▓▓▓██
      ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓██░░░░░░░░░░██▓▓                    ▓▓▓▓██
    ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓░░░░████▓▓                        ▓▓▓▓██
  ██  ██▓▓▓▓████▓▓▓▓▓▓▓▓██████▓▓▓▓▓▓              ██████      ▓▓██
  ████▓▓▓▓▓▓██  ██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓            ██      ██      ▓▓██
  ██▓▓▓▓▓▓▓▓██████▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓            ██                ▓▓██
██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓          ██                  ▓▓▓▓██
██▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓▓            ██                    ▓▓██
██▓▓▓▓▓▓▓▓▓▓▓▓▓▓████▓▓▓▓▓▓▓▓▓▓              ██                    ▓▓██
██▓▓▓▓▓▓▓▓▓▓▓▓██    ██▓▓▓▓▓▓      ██        ██                    ▓▓██
██░░██▓▓▓▓▓▓██        ██  ██        ██      ██                  ▓▓██░░██
  ██  ██████          ██░░  ████    ████████  ██    ██        ▓▓██░░░░░░██
                    ██░░░░██  ██░░  ██          ████      ██████  ██░░░░██
                      ████  ██░░░░██          ██░░░░░░████        ██░░░░██
                              ████            ████████            ██░░░░██
                    ████████████                                ██░░░░░░██
                  ██░░░░░░░░░░░░██████████████            ██████░░░░░░██
                ██░░░░██████████░░░░░░░░░░░░░░████████████░░░░░░░░░░██
                ██░░██          ██████████░░░░░░░░░░░░░░░░░░░░░░████
              ██░░██                      ██████████████████████
              ██░░██
                ██                                                        ]]


M.opts.dashboard = {
  enabled = true,
  preset = {
    header = rat,
  },
}

M.opts.indent = { enabled = true }
M.opts.quickfile = { enabled = true }
M.opts.words = { enabled = true }

M.keys = {
  {
    "<C-s><C-s>",
    mode = { "n", "t" },
    desc = "terminal-toggle",
    function()
      require("snacks").terminal.toggle(vim.env.SHELL or vim.o.shell)
    end,
  },
  {
    "<C-w>d",
    desc = "buf-delete",
    function()
      require("snacks").bufdelete.delete()
    end,
  },
  {
    "<C-w>n",
    desc = "scratch-buffer",
    function()
      require("snacks").scratch.open()
    end,
  },
}

return M
