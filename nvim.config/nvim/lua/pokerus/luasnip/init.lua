local fn = vim.fn

local ls = require "luasnip"
local types = require "luasnip.util.types"
local t = ls.text_node
local i = ls.insert_node

ls.config.set_config {
  history = true,
  updateevents = "TextChanged,TextChangedI",
  enable_autosnippets = true,
  ext_opts = {
    [types.choiceNode] = {
      active = {
        virt_text = { { "●", "DiagnosticHint" } },
      },
    },
    [types.insertNode] = {
      active = {
        virt_text = { { "●", "DiagnosticInfo" } },
      },
    },
  },
}

local shortcut = function(val)
  if type(val) == "string" then
    return { t { val }, i(0) }
  end

  if type(val) == "table" then
    for k, v in ipairs(val) do
      if type(v) == "string" then
        val[k] = t { v }
      end
    end
  end

  return val
end

local make = function(tbl)
  local result = {}
  for k, v in pairs(tbl) do
    table.insert(result, (ls.snippet({ trig = k, desc = v.desc }, shortcut(v))))
  end

  return result
end
local snippets = {}

for _, p in
  ipairs(
    fn.split(fn.glob((fn.stdpath "config") .. "/lua/pokerus/luasnip/ft/*"))
  )
do
  local ft = fn.substitute(p, "^.*/", "", "")
  ft = fn.substitute(ft, "\\.lua$", "", "")
  if ft ~= "init" then
    snippets[ft] = make(dofile(p))
  end
end

ls.add_snippets(nil, snippets, { key = "all_snippets" })
