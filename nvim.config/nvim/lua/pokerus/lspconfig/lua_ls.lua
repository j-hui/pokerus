local M = {}

M.opts = {
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

    local is_nvim_dev = false
    if vim.g.devpath then
      local nvim_devpath = vim.fn.glob(vim.fn.resolve(vim.g.devpath))
      is_nvim_dev = string.sub(new_root_dir, 1, string.len(nvim_devpath)) == nvim_devpath
    end

    -- Is this file related to Neovim/plugin developmet?
    if is_under "config" or is_under "data" or is_nvim_dev then
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

return M
