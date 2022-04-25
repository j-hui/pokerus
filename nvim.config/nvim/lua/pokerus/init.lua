-- Pokerus config/plugin management.
--
-- This module contains the entry point for Neovim-specific configuration and
-- plugins (making use of Packer). It also defines a number of helpers that are
-- useful for configuring specific plugins.
local M = {}

local fn = vim.fn

--- Neovim-specific configuration, performed before plugins are setup.
---@private
local function setup_before_plugins()
  vim.g.loaded_netrw = 1
  vim.g.loaded_netrwPlugin = 1

  vim.cmd [[
    augroup highlight_yank
      autocmd!
      autocmd TextYankPost * silent! lua vim.highlight.on_yank{higroup="CursorLine", timeout=690}
    augroup END
  ]]
end

--- Ensure Packer is installed, and use it to setup my plugins.
---@private
local function setup_packer()
  local install_path = fn.stdpath "data" .. "/site/pack/packer/opt/packer.nvim"
  local packer_need_sync

  if fn.empty(fn.glob(install_path)) > 0 then
    print "Installing packer"
    packer_need_sync = fn.system {
      "git",
      "clone",
      "--depth",
      "1",
      "https://github.com/wbthomason/packer.nvim",
      install_path,
    }
  end

  vim.cmd [[packadd packer.nvim]]

  require("packer").startup {
    function(use)
      -- "Use" packer so that it doesn't get marked for deletion
      use { "wbthomason/packer.nvim", opt = true }
      use { "lewis6991/impatient.nvim" }

      -- Automatically require any plugin module in ~/.config/nvim/lua/pokerus/plugins/
      for _, p in
        ipairs(
          fn.split(fn.glob((fn.stdpath "config") .. "/lua/pokerus/plugins/*"))
        )
      do
        p = fn.substitute(p, "^.*/", "", "")
        p = fn.substitute(p, "\\.lua$", "", "")
        require("pokerus.plugins." .. p).plug(use)
      end

      if packer_need_sync then
        -- Automatically set up configuration after cloning packer.nvim
        require("packer").sync()
      end
    end,
    config = {
      transitive_opt = false,
      profile = {
        enable = true,
        threshold = 5,
      },
    },
  }
end

--- Entry point to my Neovim configuration.
---
--- Called from init.vim.
function M.setup()
  setup_before_plugins()
  setup_packer()
end

--- Neovim-specific keybindings that aren't associated with any specific plugin.
---
--- This depends on folke/which-key.nvim, so this is invoked after that is setup.
--- (This control-flow is kind of confusing; is there a better way to do this?)
function M.setup_keybinds()
  M.nmap({
    ["]d"] = {
      "<cmd>lua vim.diagnostic.goto_next()<CR>",
      "diagnostic-next",
    },
    ["[d"] = {
      "<cmd>lua vim.diagnostic.goto_prev()<CR>",
      "diagnostic-prev",
    },
  }, { noremap = true, silent = true })

  -- These keybindings are already defined by Vim, so we just attach pretty
  -- names to them.
  M.map {
    g = {
      ["-"] = "open-parent-dir",
      ["="]= "open-cwd",
    }
  }

end

--- Define key mappings.
---
--- This is a thin wrapper around folke/which-key.nvim, and (for now) serves as
--- a layer of indirection so that I don't have direct dependencies to that
--- plugin strewn across my config.
M.map = function(mappings, opt, override_opt)
  opt = opt or {}
  override_opt = override_opt or {}
  require("which-key").register(
    mappings,
    vim.tbl_extend("force", opt, override_opt)
  )
end

--- Define normal mode mappings.
---
---@see pokerus.map
M.nmap = function(mappings, opt)
  -- M.map(mappings, opt, { mode = "n" })
  M.map(mappings, opt, {})
end

--- Define insert mode mappings.
---
---@see pokerus.map
M.imap = function(mappings, opt)
  M.map(mappings, opt, { mode = "i" })
end

--- Define visual and select mode mappings.
---
---@see pokerus.map
M.vmap = function(mappings, opt)
  M.map(mappings, opt, { mode = "v" })
end

--- Define visual mode mappings.
---
---@see pokerus.map
M.xmap = function(mappings, opt)
  M.map(mappings, opt, { mode = "x" })
end

--- Define select mode mappings.
---
---@see pokerus.map
M.smap = function(mappings, opt)
  M.map(mappings, opt, { mode = "s" })
end

--- Define operator-pending mode mappings.
---
---@see pokerus.map
M.omap = function(mappings, opt)
  M.map(mappings, opt, { mode = "o" })
end

--- Call Pokerus setup function for a Vim plugin.
M.vimsetup = function(name)
  vim.fn["pokerus#plugins#" .. name .. "#setup"]()
end

return M
