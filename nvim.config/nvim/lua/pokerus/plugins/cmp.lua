return {
  "hrsh7th/nvim-cmp",

  event = "InsertEnter",

  dependencies = {
    -- Symbols
    "onsails/lspkind-nvim",

    -- Sources
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-vsnip",
    "hrsh7th/cmp-omni",
    "hrsh7th/cmp-path",
    -- "f3fora/cmp-spell",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lua",
    "hrsh7th/cmp-nvim-lsp-signature-help",
    -- Snippets
    "L3MON4D3/LuaSnip",
    "saadparwaiz1/cmp_luasnip",
  },

  config = function()
    local cmp = require "cmp"
    local ls = require "luasnip"

    -- NOTE: default settings are reproduced here for clarity
    local mapping = {
      ["<C-n>"] = cmp.mapping.select_next_item {
        behavior = cmp.SelectBehavior.Insert,
      },
      ["<C-p>"] = cmp.mapping.select_prev_item {
        behavior = cmp.SelectBehavior.Insert,
      },
      ["<Down>"] = cmp.mapping.select_next_item {
        behavior = cmp.SelectBehavior.Select,
      },
      ["<Up>"] = cmp.mapping.select_prev_item {
        behavior = cmp.SelectBehavior.Select,
      },
      ["<M-p>"] = cmp.mapping.scroll_docs(-4),
      ["<M-n>"] = cmp.mapping.scroll_docs(4),
      ["<C-x>"] = cmp.mapping.complete(), -- show completion
      ["<C-q>"] = cmp.mapping.abort(), -- close menu, restore text
      ["<C-e>"] = cmp.mapping.close(), -- close menu, keeping text
      ["<C-l>"] = cmp.mapping.confirm { -- default was <C-y>
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      },
    }

    local function nop() end

    for _, m in ipairs { "i", "s" } do
      vim.keymap.set(m, "<C-l>",
        function()
          if ls.expand_or_jumpable() then
            ls.expand_or_jump()
          else
            mapping["<C-l>"](nop)
          end
        end,
        {
          desc = "luasnip-expand-or-jump",
          silent = true,
        }
      )

      vim.keymap.set(m, "<C-j>",
        function()
          if ls.jumpable(-1) then
            ls.jump(-1)
          end
        end,
        {
          desc = "luasnip-jump-back",
          silent = true,
        }
      )

      vim.keymap.set(m, "<C-j>",
        function()
          if ls.jumpable(-1) then
            ls.jump(-1)
          end
        end,
        {
          desc = "luasnip-change-choice",
          silent = true,
        }
      )
    end

    cmp.setup {
      snippet = {
        expand = function(args)
          require("luasnip").lsp_expand(args.body)
        end,
      },
      mapping = mapping,
      sources = cmp.config.sources({
        { name = "luasnip" },
        { name = "nvim_lua" },
        { name = "nvim_lsp" },
        { name = "nvim_lsp_signature_help" },
        { name = "path" },
      }, {
        { name = "buffer", keyword_length = 4 },
        -- { name = "spell", keyword_length = 5 },
      }),
      window = {
        documentation = cmp.config.window.bordered(),
      },
      -- formatting = {
      --   format = require'lspkind'.cmp_format({with_text = true, maxwidth = 50})
      -- },
      experimental = {
        ghost_text = true,
      },
    }
    cmp.setup.cmdline("/", {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = "buffer" },
      },
    })
    cmp.setup.cmdline(":", {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = "path", keyword_length = 2, max_item_count = 16 },
      }, {
        { name = "cmdline", keyword_length = 3, max_item_count = 16 },
      }),
    })
  end,
}
