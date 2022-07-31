local ls = require "luasnip"

---@diagnostic disable: unused-local
local s = ls.snippet
local sn = ls.snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local l = require("luasnip.extras").lambda
local rep = require("luasnip.extras").rep
local p = require("luasnip.extras").partial
local m = require("luasnip.extras").match
local n = require("luasnip.extras").nonempty
local dl = require("luasnip.extras").dynamic_lambda
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local types = require "luasnip.util.types"
local conds = require "luasnip.extras.expand_conditions"
---@diagnostic enable: unused-local

local require_var = function(args, _)
  local text = args[1][1] or ""
  local split = vim.split(text, ".", { plain = true })

  local options = {}
  for len = 0, #split - 1 do
    table.insert(
      options,
      t(table.concat(vim.list_slice(split, #split - len, #split), "_"))
    )
  end

  return sn(nil, {
    c(1, options),
  })
end

return {
  ignore = "--stylua: ignore",
  local_req = fmt([[local {} = require("{}")]], {
    d(2, require_var, { 1 }),
    i(1),
  }),
  ft_snip = fmt(
    [[
    local ls = require "luasnip"

    ---@diagnostic disable: unused-local
    local s = ls.snippet
    local sn = ls.snippet_node
    local t = ls.text_node
    local i = ls.insert_node
    local f = ls.function_node
    local c = ls.choice_node
    local d = ls.dynamic_node
    local r = ls.restore_node
    local l = require("luasnip.extras").lambda
    local rep = require("luasnip.extras").rep
    local p = require("luasnip.extras").partial
    local m = require("luasnip.extras").match
    local n = require("luasnip.extras").nonempty
    local dl = require("luasnip.extras").dynamic_lambda
    local fmt = require("luasnip.extras.fmt").fmt
    local fmta = require("luasnip.extras.fmt").fmta
    local types = require "luasnip.util.types"
    local conds = require "luasnip.extras.expand_conditions"
    ---@diagnostic enable: unused-local

    return {{
      {}
    }}
  ]],
    {i(0)}
  ),
  local_mod = fmt([[
    local {} = {{}}

    {}

    return {}
  ]], {i(1, "M"), i(0), rep(1)}),
  poke_plug = fmt(
    [[
      local M = {{}}

      function M.config()
        {}
      end

      function M.plug(use)
        use {{
          {},
          config = function()
            require("pokerus.plugins.{}").config()
          end,
        }}
      end

      return M
  ]],
    {
      i(0),
      i(1),
      f(function()
        return vim.fn.expand "%:t:r"
      end),
    }
  ),
}
