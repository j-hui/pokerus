return {
  plug = function(use)
    use {
      "hrsh7th/nvim-cmp",
      requires = {
        -- Symbols
        "onsails/lspkind-nvim",

        -- Sources
        "hrsh7th/cmp-buffer",
        "hrsh7th/cmp-path",
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-vsnip",
        "hrsh7th/cmp-omni",
        "hrsh7th/cmp-path",
        "f3fora/cmp-spell",
        "hrsh7th/cmp-cmdline",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-nvim-lsp-signature-help",
        -- Snippets
        "hrsh7th/vim-vsnip",
        "hrsh7th/vim-vsnip-integ",
      },

      config = function()
        local cmp = require "cmp"
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
          ["<C-x><C-x>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.close(),
          ["<C-y>"] = cmp.mapping.confirm {
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          },
        }

        cmp.setup {
          snippet = {
            expand = function(args)
              vim.fn["vsnip#anonymous"](args.body)
            end,
          },
          mapping = mapping,
          sources = cmp.config.sources({
            { name = "vsnip" },
            { name = "nvim_lua" },
            { name = "nvim_lsp" },
            { name = "nvim_lsp_signature_help" },
            { name = "path" },
          }, {
            { name = "buffer", keyword_length = 4 },
            { name = "spell", keyword_length = 5 },
          }),
          documentation = true,
          -- formatting = {
          --   format = require'lspkind'.cmp_format({with_text = true, maxwidth = 50})
          -- },
          experimental = {
            ghost_text = true,
          },
        }
        cmp.setup.cmdline("/", {
          sources = {
            { name = "buffer" },
          },
        })
        cmp.setup.cmdline(":", {
          sources = cmp.config.sources({
            { name = "path", keyword_length = 2, max_item_count = 16 },
          }, {
            { name = "cmdline", keyword_length = 3, max_item_count = 16 },
          }),
        })
      end,
    }
  end,
}
