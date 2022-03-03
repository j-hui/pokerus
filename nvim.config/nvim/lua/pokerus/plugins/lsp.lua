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
  -- WIP: use my own nmap for this

  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "K",
    "<cmd>lua vim.lsp.buf.hover()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>lk",
    "<cmd>lua vim.lsp.buf.hover()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>lj",
    "<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>",
    opts
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>ld",
    "<cmd>lua vim.lsp.buf.definition()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>le",
    "<cmd>lua vim.lsp.buf.declaration()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>li",
    "<cmd>lua vim.lsp.buf.implementation()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>lt",
    "<cmd>lua vim.lsp.buf.type_definition()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>l=",
    "<cmd>lua vim.lsp.buf.references()<CR>",
    opts
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>la",
    "<cmd>lua vim.lsp.buf.code_action()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "v",
    "<leader>la",
    "<cmd>lua vim.lsp.buf.range_code_action()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>lx",
    "<cmd>lua vim.lsp.codelens.run()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>lr",
    "<cmd>lua vim.lsp.buf.rename()<CR>",
    opts
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>ls",
    "<cmd>DocumentSymbols<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>l/",
    "<cmd>WorkspaceSymbols<CR>",
    opts
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "[d",
    "<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "]d",
    "<cmd>lua vim.lsp.diagnostic.goto_next()<CR>",
    opts
  )

  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>l.",
    "<cmd>TroubleToggle lsp_document_diagnostics<CR>",
    opts
  )
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "<leader>l,",
    "<cmd>TroubleToggle lsp_workspace_diagnostics<CR>",
    opts
  )

  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>', opts)
  -- vim.api.nvim_buf_set_keymap(bufnr, 'n', '<leader>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>', opts)

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
