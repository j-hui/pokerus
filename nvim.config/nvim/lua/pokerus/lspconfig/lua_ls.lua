return {
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
}
