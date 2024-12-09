return {
  "windwp/nvim-autopairs",
  event = "InsertEnter",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "hrsh7th/nvim-cmp",
  },
  config = function()
    local npairs = require("nvim-autopairs")
    npairs.setup({
      disable_filetype = { "TelescopePrompt", "markdown", "txt", "tex" },
      disable_in_macro = true,
      disable_in_visualblock = true,
      map_c_w = true, -- map <c-w> to delete a pair if possible
      check_ts = true,
      ts_config = {
        lua = { "string" },
        javascript = { "template_string" },
      },
      -- fast_wrap = {
      --   map = "<C-j>",
      -- },
    })

    -- local R = require "nvim-autopairs.rule"
    -- local cond = require "nvim-autopairs.conds"
    -- -- Adapted from: https://github.com/IndianBoy42/LunarVim/blob/0ec62e6ef5dff125065557347f840a65bea580a1/lua/lv-autopairs/init.lua
    --
    -- npairs.add_rules {
    --   R("|", "|", "rust")
    --       :with_pair(cond.not_after_regex "%%")
    --       :with_pair(cond.not_before_regex("%S", 3)), -- TODO: not working
    -- }

    -- npairs.add_rules {
    --   R("`", "'", "tex")
    --     :with_pair(cond.not_after_regex "%%")
    --     :with_pair(cond.not_before_regex("%S")), -- TODO: not working
    -- }
    --
    -- local texmods = {
    --   ["\\left"] = "\\right",
    --   ["\\big"] = "\\big",
    --   ["\\bigg"] = "\\bigg",
    --   ["\\Big"] = "\\Big",
    --   ["\\Bigg"] = "\\Bigg",
    -- }
    -- local texpairs = {
    --   ["\\("] = "\\)",
    --   ["\\["] = "\\]",
    --   ["\\{"] = "\\}",
    --   ["\\|"] = "\\|",
    --   ["\\langle "] = "\\rangle",
    --   ["\\lceil "] = "\\rceil",
    --   ["\\lfloor "] = "\\rfloor",
    -- }
    -- local basicpairs = {
    --   ["("] = ")",
    --   ["["] = "]",
    --   ["{"] = "}",
    -- }
    --
    -- for lm, rm in pairs(texmods) do
    --   for lp, rp in pairs(texpairs) do
    --     npairs.add_rule(
    --       R(lm .. lp, " " .. rm .. rp, "tex"):with_pair(cond.not_after_regex "%%")
    --     )
    --   end
    --   for lp, rp in pairs(basicpairs) do
    --     npairs.add_rule(
    --       R(lm .. lp, " " .. rm .. rp, "tex"):with_pair(cond.not_after_regex "%%")
    --     )
    --   end
    -- end
    --
    -- for lp, rp in pairs(texpairs) do
    --   npairs.add_rule(R(lp, rp, "tex"):with_pair(cond.not_after_regex "%%"))
    -- end

    -- Integration with nvim-cmp
    local cmp_autopairs = require('nvim-autopairs.completion.cmp')
    local cmp = require('cmp')
    cmp.event:on('confirm_done', cmp_autopairs.on_confirm_done())
  end,
}
