-- Pokerus config/plugin management.
--
-- This module contains the entry point for Neovim configuration and plugins.
-- It also defines helpers for configuring plugins.
local M = {}

--- Call the Pokerus setup function for a Vim plugin.
---@param name string
M.vimsetup = function(name)
  vim.fn["pokerus#plugins#" .. name .. "#setup"]()
end

local function ensure_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  ---@diagnostic disable-next-line: undefined-field
  if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/folke/lazy.nvim.git",
      "--branch=stable", -- latest stable release
      lazypath,
    })
  end
  vim.opt.rtp:prepend(lazypath)
end

function M.setup()
  ensure_lazy()

  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1

  vim.g.mapleader = " "
  vim.g.devpath = "~/Documents/nvim-dev"

  local colorscheme = require("pokerus.colorscheme")
  colorscheme.setup()

  require("lazy").setup({
    { import = "pokerus.plugins" },
    colorscheme.colorschemes,
  }, {
    install = { colorscheme = { colorscheme.main, "habamax" } },
    dev = {
      path = vim.g.devpath,
      fallback = true,
    },
    checker = {
      enabled = true,
      concurrency = 4,
    },
    change_detection = {
      enabled = false,
      notify = false,
    },
    performance = {
      rtp = {
        reset = false,
        disabled_plugins = {
          "gzip",
          "matchit",     -- supplanted by vim-matchup
          "matchparen",  -- supplanted by vim-matchup
          "netrwPlugin", -- supplanted by lir.nvim
          "rplugin",
          "tarPlugin",
          "tohtml",
          "tutor",
          "zipPlugin",
        },
      }
    },
  })

  vim.fn["pokerus#settings#setup"]()
  vim.fn["pokerus#keybinds#setup"]()
  require("pokerus.keybinds").setup()
  require("pokerus.lsp").setup()
  require("pokerus.ft").setup()
  vim.fn["pokerus#commands#setup"]()
end

return M
