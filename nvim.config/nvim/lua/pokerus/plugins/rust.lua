local use_ferris = true

local ferris = {
  "vxpm/ferris.nvim",
  dependencies = {
    "neovim/nvim-lspconfig",
  },
  opts = {},
  ft = "rust",
}

local rustaceanvim = {
  "mrcjkb/rustaceanvim",
  version = "^4",
  dependencies = {
    "mfussenegger/nvim-dap",
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },
  ft = "rust",

  config = function()
    -- NOTE: See how to configure different settings per project here: https://github.com/mrcjkb/rustaceanvim?tab=readme-ov-file#how-to-dynamically-load-different-rust-analyzer-settings-per-project

    local ra_opts = require("pokerus.lspconfig.rust_analyzer").opts or {}

    vim.g.rustaceanvim = {
      -- LSP configuration
      server = {
        on_attach = function(client, bufnr)
          require("pokerus.lsp").on_attach(client, bufnr)
        end,
        default_settings = ra_opts.settings,
      },
    }

    -- Look for vscode-lldb extension
    local extension_path = vim.fn.globpath(vim.env.HOME .. "/.vscode/extensions", "vadimcn.vscode-lldb-*", false, true)
    if #extension_path > 0 then
      if #extension_path > 1 then
        vim.notify("Multiple versions of vscode-lldb appear to be installed!\n" .. vim.inspect(extension_path),
          vim.log.levels.WARN)
      end

      local codelldb_path = extension_path[1] .. 'adapter/codelldb'
      local liblldb_path = extension_path[1] .. 'lldb/lib/liblldb'
      local this_os = vim.loop.os_uname().sysname;

      -- The liblldb extension is .so for linux and .dylib for macOS
      liblldb_path = liblldb_path .. (this_os == "Linux" and ".so" or ".dylib")

      -- Set dap configuration
      vim.g.rustaceanvim.dap = {
        adapter = require("rustaceanvim.config").get_codelldb_adapter(codelldb_path, liblldb_path)
      }
    end
  end,
}

if use_ferris then
  return ferris
else
  return rustaceanvim
end
