-- don't forget about the lsp-colors thing
local M = {}

M.servers = {
  ["clangd"] = {},
  ["texlab"] = {
    settings = {
      texlab = {
        build = { onSave = true },
        chkTex = { onOpenAndSave = true },
        forwardSearch = {
          executable = "zathura",
          args = { "--synctex-forward", "%l:1:%f", "%p" },
        },
      },
    },
  },
  ["rust_analyzer"] = {},
  ["pyright"] = {},
  ["tsserver"] = {},
  ["hls"] = { settings = { haskell = { formattingProvider = "brittany" } } },
  ["vimls"] = {},
  ["ocamllsp"] = {},
  ["rnix"] = {},
  ["gopls"] = {},
  ["sumneko_lua"] = {
    root_dir = function(filename) -- (extra arg: bufnr)
      local util = require "lspconfig.util"
      local primary = util.root_pattern ".luarc.json"(filename)
      local backup = util.find_git_ancestor(filename)
      return primary or backup
    end,
  },
}

function M.on_attach(client, bufnr)
  require("pokerus").nmap({
    name = "lsp",
    k = { "lsp-hover", "<cmd>lua vim.lsp.buf.hover()<CR>" },
    j = {
      "lsp-show-diagnostic",
      "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>",
    },
    d = { "lsp-goto-definition", "<cmd>lua vim.lsp.buf.definition()<CR>" },
    e = { "lsp-goto-declaration", "<cmd>lua vim.lsp.buf.declaration()<CR>" },
    i = {
      "lsp-goto-implementation",
      "<cmd>lua vim.lsp.buf.implementation()<CR>",
    },
    t = { "lsp-goto-typedef", "<cmd>lua vim.lsp.buf.type_definition()<CR>" },
    ["="] = { "lsp-references", "<cmd>lua vim.lsp.buf.references()<CR>" },
    a = { "lsp-code-action", "<cmd>lua vim.lsp.buf.code_action()<CR>" },
    x = { "lsp-lens-code-action", "<cmd>lua vim.lsp.buf.codelens.run()<CR>" },
    r = { "lsp-rename", "<cmd>lua vim.lsp.buf.rename()<CR>" },
  }, { prefix = "<leader>l", noremap = true, silent = true, buffer = bufnr })

  require("pokerus").nmap({
    K = { "lsp-hover", "<cmd>lua vim.lsp.buf.hover()<CR>" },

    ["]d"] = {
      "lsp-diagnostic-next",
      "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
    },
    ["[d"] = {
      "lsp-diagnostic-prev",
      "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>",
    },
  }, { noremap = true, silent = true, buffer = bufnr })

  require("pokerus").vmap {
    ["<leader>la"] = {
      "lsp-code-action",
      "<cmd>lua vim.lsp.buf.code_action()<CR>",
      noremap = true,
      silent = true,
      buffer = bufnr,
    },
  }

  require("pokerus").vmap {
    ["<leader>lf"] = {
      "lsp-outline",
      "<cmd>AerialToggle<CR>",
      noremap = true,
      silent = true,
      buffer = bufnr,
    },
  }

  -- vim.cmd [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]] -- NOTE causes weird issue sometimes but that can be ignored
  vim.cmd [[command! Fmt lua vim.lsp.buf.formatting()]]

  vim.api.nvim_buf_set_keymap(
    0,
    "n",
    "<leader>lo",
    "<cmd>AerialToggle!<CR>",
    {}
  )
  vim.api.nvim_buf_set_keymap(0, "n", "[a", "<cmd>AerialPrev<CR>", {})
  vim.api.nvim_buf_set_keymap(0, "n", "]a", "<cmd>AerialNext<CR>", {})
  vim.api.nvim_buf_set_keymap(0, "n", "[[", "<cmd>AerialPrevUp<CR>", {})
  vim.api.nvim_buf_set_keymap(0, "n", "]]", "<cmd>AerialNextUp<CR>", {})

  if vim.bo.filetype == "tex" then
    vim.api.nvim_buf_set_keymap(
      bufnr,
      "n",
      "<leader>lc",
      "<cmd>TexlabBuild<CR>",
      opts
    )
    vim.api.nvim_buf_set_keymap(
      bufnr,
      "n",
      "<leader>lf",
      "<cmd>TexlabForward<CR>",
      opts
    )
  end

  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  require("aerial").on_attach(client)
end

function M.rust_setup()
  vim.cmd [[
    augroup nvimlsp_extensions_rust
      autocmd!
      autocmd CursorHold,CursorHoldI <buffer> lua require'lsp_extensions'.inlay_hints{ only_current_line = true }
    augroup END
  ]]
end

function M.plug(use)
  use {
    "neovim/nvim-lspconfig",
    requires = {
      "nvim-lua/lsp_extensions.nvim",
      -- Better support ofr some languages
      "weilbith/nvim-lsp-smag",
      -- Override tagfunc, use C-] to jump to definition
      "folke/lua-dev.nvim",
      -- Nvim lua development
      "stevearc/aerial.nvim",
      -- Code outline viewer
      "nvim-treesitter/nvim-treesitter",
      -- Required for Aerial
    },
    config = function()
      local nvim_lsp = require "lspconfig"

      M.servers["sumneko_lua"] = require("lua-dev").setup {
        lspconfig = M.servers["sumneko_lua"],
      }

      require("aerial").setup {
        manage_folds = false,
        backends = {
          ["_"] = { "lsp", "treesitter" },
          ["bib"] = { "treesitter" },
        },
      }

      for lsp, cfg in pairs(M.servers) do
        nvim_lsp[lsp].setup(vim.tbl_extend("keep", cfg, {
          on_attach = M.on_attach,
          capabilities = require("cmp_nvim_lsp").update_capabilities(
            vim.lsp.protocol.make_client_capabilities()
          ),
        }))
      end

      vim.cmd [[
        augroup nvimlsp_extensions
          autocmd!
          autocmd FileType rust lua require("pokerus.lsp").rust_setup()
        augroup END

        sign define LspDiagnosticsSignError text=✖ texthl=LspDiagnosticsSignError
        sign define LspDiagnosticsSignWarning text=‼ texthl=LspDiagnosticsSignWarning
        sign define LspDiagnosticsSignInformation text=ℹ texthl=LspDiagnosticsSignInformation
        sign define LspDiagnosticsSignHint text=» texthl=LspDiagnosticsSignHint
      ]]
    end,
  }
end

return M
