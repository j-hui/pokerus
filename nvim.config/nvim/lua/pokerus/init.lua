-- Pokerus config/plugin management.
--
-- This module contains the entry point for Neovim configuration and plugins.
-- It also defines helpers for configuring plugins.
local M = {}

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

--- Neovim-specific settings that aren't associated with any specific plugin.
local function setup_nvim_colorscheme()
  local signs = {
    { name = "DiagnosticSignError", text = "✖" },
    { name = "DiagnosticSignWarn", text = "‼" },
    { name = "DiagnosticSignHint", text = "ℹ" },
    { name = "DiagnosticSignInfo", text = "»" },
  }

  for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end

  vim.api.nvim_set_hl(0, "LspReferenceRead", { underline = true })
  vim.api.nvim_set_hl(0, "LspReferenceWrite", { underline = true })
  vim.api.nvim_set_hl(0, "LspReferenceText", { underline = true })
end

local function setup_nvim_ftdetect()
  vim.filetype.add {
    filename = {
      justfile = "just",
      [".envrc"] = "sh",
    },
    extension = {
      h = "cpp",
      v = "coq",
      x = "alex",
      y = "happy",
      pio = "pio",
      luau = "luau",
      rbxlx = "xml",
      pbxproj = "pbxproj",
      modulemap = "modulemap",
    },
  }
end

local function setup_nvim_editorconfig()
  pcall(function()
    ---@diagnostic disable-next-line: duplicate-set-field
    require("editorconfig").properties.trim_trailing_whitespace = function()
      -- disable this specific behavior. it screws up my diffs.
    end
  end)
end

--- Entry point to my Neovim configuration.
---
--- Called from init.vim.
function M.setup()
  ensure_lazy()
  require("pokerus.profile").setup()

  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1

  vim.g.mapleader = " "
  vim.g.devpath = "~/Documents/nvim-dev"

  local colorscheme = require "pokerus.colorscheme"

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

  setup_nvim_colorscheme()
  require("pokerus.callback").colorscheme(setup_nvim_colorscheme)

  vim.fn["pokerus#keybinds#setup"]()
  require("pokerus.keybinds").setup()

  require("pokerus.lsp").setup()

  setup_nvim_ftdetect()

  setup_nvim_editorconfig()

  vim.fn["pokerus#commands#setup"]()

  require("pokerus.scratch").setup()

  -- Done by yanky.nvim
  -- vim.cmd [[
  --   augroup highlight_yank
  --     autocmd!
  --     autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="CursorLine", timeout=690}
  --   augroup END
  -- ]]

  if require("profile").is_recording() then
    require("profile").log_instant("pokerus:setup_end")
  end
end

--- Call the Pokerus setup function for a Vim plugin.
---@param name string
M.vimsetup = function(name)
  vim.fn["pokerus#plugins#" .. name .. "#setup"]()
end

return M
