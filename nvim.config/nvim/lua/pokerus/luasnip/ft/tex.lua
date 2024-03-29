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

local function rec_ls()
  return sn(nil, {
    c(1, {
      -- important!! Having the sn(...) as the first choice will cause infinite recursion.
      t { "" },
      -- The same dynamicNode as in the snippet (also note: self reference).
      sn(nil, { t { "", "\t\\item " }, i(1), d(2, rec_ls, {}) }),
    }),
  })
end

return {
  items = {
    t { "\\begin{itemize}", "\t\\item " },
    i(1),
    d(2, rec_ls, {}),
    t { "", "\\end{itemize}" },
    i(0),
  },
  begin = fmt(
    [[
      \begin{{{}}}
        {}
      \end{{{}}}
    ]],
    { i(1, "env"), i(0), rep(1) }
  ),
}
