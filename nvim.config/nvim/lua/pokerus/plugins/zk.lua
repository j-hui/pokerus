local M = {}

function M.config()
  require("zk").setup {
    lsp = {
      config = {
        cmd = { "zk", "lsp" },
        name = "zk",
        on_attach = function(client, bufnr)
          require("pokerus.plugins.lsp").on_attach(client, bufnr)
          require("pokerus").nmap({
            ["."] = {
              "<cmd>ZkNotes<CR>",
              "zk-cd",
            },
            n = {
              "<cmd>ZkNotes<CR>",
              "zk-notes",
            },
            f = {
              "<cmd>ZkLinks<CR>",
              "zk-links",
            },
            b = {
              "<cmd>ZkBacklinks<CR>",
              "zk-backlinks",
            },
          }, {
            prefix = "<leader>l",
            noremap = true,
            silent = true,
            buffer = bufnr,
          })
          require("pokerus").xmap({
            p = {
              ":'<,'>ZkNewFromTitleSelection { dir = 'person' }<CR>",
              "zk-new-from-title-selection",
            },
            t = {
              ":'<,'>ZkNewFromTitleSelection<CR>",
              "zk-new-from-title-selection",
            },
          }, {
            prefix = "<leader>l",
            noremap = true,
            silent = true,
            buffer = bufnr,
          })
        end,
      },
    },
  }

  local zk = require "zk"
  local commands = require "zk.commands"
  local function make_edit_fn(defaults, picker_options)
    return function(options)
      options = vim.tbl_extend("force", defaults, options or {})
      zk.edit(options, picker_options)
    end
  end
  commands.add(
    "ZkOrphans",
    make_edit_fn({ orphan = true }, { title = "Zk Orphans" })
  )
  commands.add(
    "ZkRecents",
    make_edit_fn({ createdAfter = "2 weeks ago" }, { title = "Zk Recents" })
  )
end

function M.plug(use)
  use {
    "mickael-menu/zk-nvim",
    opt = true,
    event = "VimEnter",
    config = function()
      require("pokerus.plugins.zk").config()
    end,
  }
end

return M
