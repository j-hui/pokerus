local M = {}

M.servers = {
  ["clangd"] = {},
  ["texlab"] = {
    on_attach = function(client, bufnr)
      require("pokerus.plugins.lsp").on_attach(client, bufnr)
      require("pokerus").nmap(
        {
          c = {
            "<cmd>TexlabBuild<CR>",
            "tex-build",
          },
          f = {
            "<cmd>TexlabForward<CR>",
            "tex-goto-line",
          },
        },
        { prefix = "<leader>l", noremap = true, silent = true, buffer = bufnr }
      )
    end,
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
  ["hls"] = {
    settings = { haskell = { formattingProvider = "brittany" } },
  },
  ["vimls"] = {},
  ["ocamllsp"] = {},
  ["rnix"] = {},
  ["gopls"] = {},
  ["sumneko_lua"] = {
    settings = {
      Lua = {
        diagnostics = {
          disable = {
            "trailing-space",
          },
        },
      },
    },
    on_new_config = function(new_config, new_root_dir)
      local function is_under(path)
        path = vim.fn.resolve(vim.fn.stdpath(path))
        return string.sub(new_root_dir, 1, string.len(path)) == path
      end

      -- Is this file related to Neovim/plugin developmet?
      if is_under "config" or is_under "data" then
        local luasettings = require("lua-dev").setup {
          library = { plugins = false }, -- I have too many plugins xD
          lspconfig = { settings = new_config.settings },
        }
        new_config.settings = luasettings.settings
      end
    end,
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
    k = {
      "<cmd>lua vim.lsp.buf.hover()<CR>",
      "lsp-hover",
    },
    d = {
      "<cmd>lua vim.lsp.buf.definition()<CR>",
      "lsp-goto-definition",
    },
    e = {
      "<cmd>lua vim.lsp.buf.declaration()<CR>",
      "lsp-goto-declaration",
    },
    i = {
      "<cmd>lua vim.lsp.buf.implementation()<CR>",
      "lsp-goto-implementation",
    },
    t = {
      "<cmd>lua vim.lsp.buf.type_definition()<CR>",
      "lsp-goto-typedef",
    },
    ["="] = {
      "<cmd>lua vim.lsp.buf.references()<CR>",
      "lsp-references",
    },
    a = {
      "<cmd>lua vim.lsp.buf.code_action()<CR>",
      "lsp-code-action",
    },
    x = {
      "<cmd>lua vim.lsp.codelens.run()<CR>",
      "lsp-lens-code-action",
    },
    r = {
      "<cmd>lua vim.lsp.buf.rename()<CR>",
      "lsp-rename",
    },
  }, {
    prefix = "<leader>l",
    noremap = true,
    silent = true,
    buffer = bufnr,
  })

  require("pokerus").nmap({
    K = {
      "<cmd>lua vim.lsp.buf.hover()<CR>",
      "lsp-hover",
    },
  }, { noremap = true, silent = true, buffer = bufnr })

  require("pokerus").xmap {
    ["<leader>la"] = {
      "<cmd>lua vim.lsp.buf.code_action()<CR>",
      "lsp-code-action",
      noremap = true,
      silent = true,
      buffer = bufnr,
    },
  }

  require("pokerus").xmap {
    ["<leader>lo"] = {
      "<cmd>AerialToggle!<CR>",
      "lsp-outline",
      noremap = true,
      silent = true,
      buffer = bufnr,
    },
  }

  -- vim.cmd [[autocmd CursorHold,CursorHoldI,InsertLeave <buffer> lua vim.lsp.codelens.refresh()]]
  -- ^NOTE: causes weird issue sometimes but that can be ignored

  vim.cmd [[command! Fmt lua vim.lsp.buf.format {async = true}]]

  vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
  require("aerial").on_attach(client)
end

function M.rust_setup()
  require("rust-tools").setup {
    server = {
      on_attach = M.on_attach,
    },
  }
end

function M.config()
  local nvim_lsp = require "lspconfig"

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
    augroup nvimlsp_rust
      autocmd!
      autocmd FileType rust lua require("pokerus.plugins.lsp").rust_setup()
    augroup END

    sign define LspDiagnosticsSignError text=✖ texthl=LspDiagnosticsSignError
    sign define LspDiagnosticsSignWarning text=‼ texthl=LspDiagnosticsSignWarning
    sign define LspDiagnosticsSignInformation text=ℹ texthl=LspDiagnosticsSignInformation
    sign define LspDiagnosticsSignHint text=» texthl=LspDiagnosticsSignHint
  ]]
end

function M.plug(use)
  use {
    "neovim/nvim-lspconfig",
    requires = {
      "weilbith/nvim-lsp-smag",
      -- Override tagfunc, use C-] to jump to definition
      "folke/lua-dev.nvim",
      -- Nvim lua development
      "stevearc/aerial.nvim",
      -- Code outline viewer
      "nvim-treesitter/nvim-treesitter",
      -- Required for Aerial
      "folke/lsp-colors.nvim",
      -- Creates missing LSP diagnostics highlight groups
      "simrat39/rust-tools.nvim",
      -- Rust-specific support
    },
    config = function()
      require("pokerus.plugins.lsp").config()
    end,
  }
end

return M
