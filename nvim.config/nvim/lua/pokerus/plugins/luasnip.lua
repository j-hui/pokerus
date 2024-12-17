local M = {}

function M.snip_path(ft)
  local path = (vim.fn.stdpath "config") .. "/lua/pokerus/luasnip/"
  if ft == nil or ft == ".init" then
    path = path .. "init.lua"
  else
    path = path .. "ft/" .. ft .. ".lua"
  end
  return path
end

function M.edit(args)
  if args.args then
    vim.cmd("edit " .. M.snip_path(args.args))
  else
    local fts = { "all", ".init" }

    local cur_ft = vim.api.nvim_get_option_value("filetype", {})
    if type(cur_ft) == "string" and #cur_ft > 0 then
      table.insert(fts, 1, cur_ft)
    end

    vim.ui.select(fts, { prompt = "Luasnip filetype:" }, function(choice)
      if choice then
        vim.cmd("edit " .. M.snip_path(choice))
      end
    end)
  end
end

function M.source()
  vim.cmd("source " .. M.snip_path())
  require("luasnip.loaders.from_snipmate").lazy_load({ path = "./snippets" })
end

function M.forward()
  local ls = require("luasnip")
  if ls.jumpable(1) then
    ls.jump(1)
  else
    vim.cmd("normal! w")
  end
end

function M.backward()
  local ls = require("luasnip")
  if ls.jumpable(-1) then
    ls.jump(-1)
  else
    vim.cmd("normal! b")
  end
end

function M.clear()
  require("luasnip").unlink_current()
end

return {
  "L3MON4D3/LuaSnip",
  event = "InsertEnter",
  dependencies = {
    "rafamadriz/friendly-snippets",
  },
  keys = {
    { "<M-f>", mode = { "n", "s", "i" }, M.forward,  desc = "luasnip-next" },
    { "<M-b>", mode = { "n", "s", "i" }, M.backward, desc = "luasnip-prev" },
    { "<M-l>", mode = { "n", "s", "i" }, M.clear,    desc = "luasnip-clear" },
  },
  config = function()
    vim.api.nvim_create_user_command("LuaSnipEdit", M.edit, { nargs = "?", complete = "filetype" })
    vim.api.nvim_create_user_command("LSE", M.edit, { nargs = "?", complete = "filetype" })
    vim.api.nvim_create_user_command("LuaSnipRefresh", M.source, {})
    vim.api.nvim_create_user_command("LSE", M.source, {})
    M.source()
  end,
}
