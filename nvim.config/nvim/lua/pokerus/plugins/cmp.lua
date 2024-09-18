local M = {
  "hrsh7th/nvim-cmp",
  event = "InsertEnter",
  dependencies = {
    -- Symbols
    "onsails/lspkind-nvim",

    -- Sources
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-nvim-lsp",
    -- "hrsh7th/cmp-vsnip",
    "hrsh7th/cmp-omni",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-nvim-lsp-document-symbol",
    "hrsh7th/cmp-cmdline",
    "hrsh7th/cmp-nvim-lua",
    -- "hrsh7th/cmp-nvim-lsp-signature-help",
    -- Snippets
    "L3MON4D3/LuaSnip",
    "saadparwaiz1/cmp_luasnip",
  },
}

M.sources = {
  { name = "luasnip" },
  { name = "nvim_lua" },
  { name = "nvim_lsp" },
  { name = "path" },
}

M.fallback_sources = {
  {
    name = "buffer",
    option = {
      keyword_length = 5,
      get_bufnrs = function()
        -- Complete from all bufs
        return vim.api.nvim_list_bufs()
        -- -- Only complete from visible bufs
        -- local bufs = {}
        -- for _, win in ipairs(vim.api.nvim_list_wins()) do
        --   bufs[vim.api.nvim_win_get_buf(win)] = true
        -- end
        -- return vim.tbl_keys(bufs)
      end
    }
  }
}

function M.mapping()
  local cmp = require("cmp")
  local ls = require("luasnip")

  return {
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
    -- ["<C-x>"] = cmp.mapping.complete(), -- show completion
    ["<C-q>"] = cmp.mapping.abort(), -- close menu, restore text
    ["<C-e>"] = cmp.mapping.close(), -- close menu, keeping text
    ["<C-l>"] = cmp.mapping(function(fallback)
      if ls.expand_or_jumpable() then
        ls.expand_or_jump()
      elseif cmp.visible() then
        cmp.confirm({ -- default was <C-y>
          behavior = cmp.ConfirmBehavior.Replace,
          select = true,
        })
      else
        fallback()
      end
    end),
  }
end

function M.config()
  local cmp = require("cmp")

  cmp.setup({
    enabled = function()
      -- Only enable cmp when the buffer is a real file
      local buftype = vim.api.nvim_get_option_value("buftype", { buf = 0 })
      if buftype == "nofile" then
        return false
      end

      local ft = vim.api.nvim_get_option_value("filetype", { buf = 0 })
      if ft == "TelescopePrompt" then
        return false
      end

      return true
    end,
    snippet = {
      expand = function(args)
        require("luasnip").lsp_expand(args.body)
      end,
    },
    mapping = M.mapping(),
    sources = cmp.config.sources(M.sources, M.fallback_sources),
    window = {
      documentation = cmp.config.window.bordered(),
    },
    -- experimental = {
    --   ghost_text = true,
    -- },
  })

  cmp.setup.cmdline("/", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = "nvim_lsp_document_symbol" },
      { name = "buffer" },
    })
  })

  cmp.setup.cmdline(":", {
    mapping = cmp.mapping.preset.cmdline(),
    sources = cmp.config.sources({
      { name = "path", keyword_length = 2, max_item_count = 16 },
    }, {
      { name = "cmdline", keyword_length = 3, max_item_count = 16 },
    }),
  })
end

return M
