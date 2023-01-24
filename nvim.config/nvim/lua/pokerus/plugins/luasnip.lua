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

    local cur_ft = vim.api.nvim_buf_get_option(0, "filetype")
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

return {
  "L3MON4D3/LuaSnip",
  event = "InsertEnter",
  dependencies = {
    "rafamadriz/friendly-snippets",
  },
  config = function()
    vim.api.nvim_create_user_command("LuaSnipEdit", M.edit, { nargs = "?", complete = "filetype" })
    vim.api.nvim_create_user_command("LSE", M.edit, { nargs = "?", complete = "filetype" })
    vim.api.nvim_create_user_command("LuaSnipRefresh", M.source, {})
    vim.api.nvim_create_user_command("LSE", M.source, {})

    M.source()
  end,
}
