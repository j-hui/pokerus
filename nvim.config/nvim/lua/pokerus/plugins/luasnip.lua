local M = {}

function M.snip_path(ft)
  if ft == ".init" then
    return (vim.fn.stdpath "config") .. "/lua/pokerus/luasnip/init.lua"
  else
    return (vim.fn.stdpath "config")
      .. "/lua/pokerus/luasnip/ft/"
      .. ft
      .. ".lua"
  end
end

function M.edit(ft)
  if ft then
    vim.cmd("edit" .. M.snip_path(ft))
  else
    local fts = { "all", ".init" }

    local cur_ft = vim.api.nvim_buf_get_option(0, "filetype")
    if type(cur_ft) == "string" and #cur_ft > 0 then
      table.insert(fts, 1, cur_ft)
    end

    vim.ui.select(fts, { prompt = "Luasnip filetype:" }, function(choice)
      if choice then
        vim.cmd("edit" .. M.snip_path(choice))
      end
    end)
  end
end

function M.source()
  vim.cmd(
    "source " .. (vim.fn.stdpath "config") .. "/lua/pokerus/luasnip/init.lua"
  )
end

function M.config()
  vim.cmd [[
    command! -nargs=? -complete=filetype LuaSnipEdit :lua require("pokerus.plugins.luasnip").edit(<f-args>)
    command! -nargs=? -complete=filetype LSE :lua require("pokerus.plugins.luasnip").edit(<f-args>)
    command! LuaSnipRefresh :lua require("pokerus.plugins.luasnip").source()
    command! LSR :lua require("pokerus.plugins.luasnip").source()
  ]]

  M.source()
end

function M.plug(use)
  use {
    "L3MON4D3/LuaSnip",
    event = "VimEnter",
    requires = { "rafamadriz/friendly-snippets" },
    config = function()
      require("pokerus.plugins.luasnip").config()
    end,
  }
end

return M
