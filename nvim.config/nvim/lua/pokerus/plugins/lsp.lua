-- TODO: see how to decouple these configs from lsp.lua
local lsp_servers = {
  ["clangd"] = {},
  ["texlab"] = {
    on_attach = function(client, bufnr)
      require("pokerus.lsp").on_attach(client, bufnr)
      vim.keymap.set("n", "<leader>lc", "<cmd>TexlabBuild<CR>", { desc = "tex-build" })
      vim.keymap.set("n", "<leader>lf", "<cmd>TexlabForward<CR>", { desc = "tex-goto-line" })
    end,
    settings = {
      texlab = {
        build = { onSave = true },
        chkTex = { onOpenAndSave = true },
        forwardSearch = (function()
          local skim_path = "/Applications/Skim.app/Contents/SharedSupport/displayline"
          if vim.fn.executable(skim_path) then
            return {
              executable = skim_path,
              args = { "-g", "%l", "%p", "%f" },
            }
          else
            return {
              executable = "zathura",
              args = { "--synctex-forward", "%l:1:%f", "%p" },
            }
          end
        end)()
      },
    },
  },
  ["pyright"] = {},
  ["tsserver"] = {},
  ["hls"] = {
    settings = { haskell = { formattingProvider = "fourmolu" } },
  },
  ["vimls"] = {},
  ["ocamllsp"] = {},
  ["rnix"] = {},
  ["gopls"] = {},
  ["sumneko_lua"] = {
    settings = {
      Lua = {
        diagnostics = {
          disable = {
            "trailing-space",
          },
        },
      },
    },
    on_new_config = function(new_config, new_root_dir)
      local function is_under(path)
        path = vim.fn.resolve(vim.fn.stdpath(path))
        return string.sub(new_root_dir, 1, string.len(path)) == path
      end

      -- Is this file related to Neovim/plugin developmet?
      if is_under "config" or is_under "data" then
        local luasettings = require("neodev").setup {
          library = { plugins = false }, -- I have too many plugins xD
          lspconfig = { settings = new_config.settings },
        }
        new_config.settings = luasettings.settings
      end
    end,
    root_dir = function(filename) -- (extra arg: bufnr)
      local util = require "lspconfig.util"
      local primary = util.root_pattern ".luarc.json" (filename)
      local backup = util.find_git_ancestor(filename)
      return primary or backup
    end,
  },
}

return {
  "neovim/nvim-lspconfig",
  dependencies = {
    "weilbith/nvim-lsp-smag",
    -- Override tagfunc, use C-] to jump to definition

    "folke/neodev.nvim",
    -- Nvim lua development

    "folke/lsp-colors.nvim",
    -- Creates missing LSP diagnostics highlight groups

    "hrsh7th/nvim-cmp",
    "hrsh7th/cmp-nvim-lsp",
    -- Completion suggestions
  },
  config = function()
    for lsp, cfg in pairs(lsp_servers) do
      require("pokerus.lsp").setup(lsp, cfg)
    end
  end,
}
