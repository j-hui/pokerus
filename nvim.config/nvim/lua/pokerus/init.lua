-- Pokerus config/plugin management.
--
-- This module contains the entry point for Neovim configuration and plugins.
-- It also defines helpers for configuring plugins.
local M = {}

local function ensure_lazy()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
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

local nvim_use_virtual_text = true

local function nvim_configure_diagnostics()
  local virtual_text_config = {
    severity = {
      min = vim.diagnostic.severity.WARN,
    }
  }
  if nvim_use_virtual_text then
    vim.diagnostic.config({
      severity_sort = true,
      virtual_text = virtual_text_config
    })
  else
    vim.diagnostic.config({
      severity_sort = true,
      virtual_text = false
    })
  end
  nvim_use_virtual_text = not nvim_use_virtual_text
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
end

--- Neovim-specific keybindings that aren't associated with any specific plugin.
local function setup_nvim_keybinds()
  local function goto_next_diagnostic()
    vim.diagnostic.goto_next({ severity = { min = vim.diagnostic.severity.WARN } })
  end

  local function goto_prev_diagnostic()
    vim.diagnostic.goto_prev({ severity = { min = vim.diagnostic.severity.WARN } })
  end

  vim.keymap.set("n", "]d", goto_next_diagnostic, { desc = "diagnostic-next", silent = true })
  vim.keymap.set("n", "[d", goto_prev_diagnostic, { desc = "diagnostic-prev", silent = true })

  vim.keymap.set("n", "<leader>lj", vim.diagnostic.open_float, { desc = "diagnostic-show", silent = true })
  vim.keymap.set("n", "<leader>lL", nvim_configure_diagnostics, { desc = "virtual-text-toggle", silent = true })

  vim.keymap.set("n", "<leader>+", ":lua ", { desc = "lua-run" })
  vim.keymap.set("n", "<leader>=", ":lua =()<left>", { desc = "lua-eval" })

  vim.keymap.set("n", "g-", ":edit %:h<CR>", { desc = "open-.." })
  vim.keymap.set("n", "g=", ":edit .<CR>", { desc = "open-cwd" })
end

local function setup_nvim_ftdetect()
  vim.filetype.add {
    filename = {
      justfile = "just",
    },
    extension = {
      h = "c",
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

--- Entry point to my Neovim configuration.
---
--- Called from init.vim.
function M.setup()
  ensure_lazy()

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
  nvim_configure_diagnostics()

  setup_nvim_colorscheme()
  require("pokerus.callback").colorscheme(setup_nvim_colorscheme)

  vim.fn["pokerus#keybinds#setup"]()
  setup_nvim_keybinds()

  setup_nvim_ftdetect()

  vim.fn["pokerus#commands#setup"]()

  require("pokerus.scratch").setup()

  -- Done by yanky.nvim
  -- vim.cmd [[
  --   augroup highlight_yank
  --     autocmd!
  --     autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="CursorLine", timeout=690}
  --   augroup END
  -- ]]
end

--- Call the Pokerus setup function for a Vim plugin.
---@param name string
M.vimsetup = function(name)
  vim.fn["pokerus#plugins#" .. name .. "#setup"]()
end

return M
