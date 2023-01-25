return {
  "mickael-menu/zk-nvim",
  opt = true,
  event = "VeryLazy",
  config = function()
    require("zk").setup {
      lsp = {
        config = {
          cmd = { "zk", "lsp" },
          name = "zk",
          on_attach = function(client, bufnr)
            require("pokerus.lsp").on_attach(client, bufnr)

            local function map(m, l, r, d)
              vim.keymap.set(m, l, r, { desc = "zk-" .. d, buffer = bufnr })
            end

            map("n", "<leader>l.", "<cmd>ZkCd<CR>", "cd")
            map("n", "<leader>ln", "<cmd>ZkNotes<CR>", "notes")
            map("n", "<leader>lf", "<cmd>ZkLinks<CR>", "links")
            map("n", "<leader>lb", "<cmd>ZkBacklinks<CR>", "backlinks")
            map("x", "<leader>lp", [[:'<,'>ZkNewFromTitleSelection { dir = 'person' }<CR>]], "new-person")
            map("x", "<leader>lt", [[:'<,'>ZkNewFromTitleSelection<CR>]], "new-note")
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
  end,
}
