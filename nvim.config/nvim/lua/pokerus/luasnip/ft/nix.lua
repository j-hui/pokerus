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

return {
  poke_mod = fmt(
    [[
    {{ config, pkgs, lib, ... }}:
    with lib;
    let
      moduleName = "{}";
      cfg = config.pokerus.${{moduleName}};
    in
    {{
      imports = [
      ];

      options.pokerus.${{moduleName}} = {{
        enable = mkEnableOption "{}";
      }};

      config = mkIf cfg.enable {{
        {}
      }};
    }}
  ]],
    { i(1, "module"), i(2, "module description"), i(0) }
  ),
  poke_pkg = fmt(
    [[
    {{ config, pkgs, lib, ... }}:
    with lib;
    let
      moduleName = "{}";
      cfg = config.pokerus.${{moduleName}};
    in
    {{
      imports = [
      ];

      options.pokerus.${{moduleName}} = {{
        enable = mkEnableOption "{}";
      }};

      config = mkIf cfg.enable {{
        environment.systemPackages = with pkgs; [
          {}
        ];{}
      }};
    }}
  ]],
    { i(1, "module"), i(2, "module description"), i(3, ""), i(0) }
  ),
}
