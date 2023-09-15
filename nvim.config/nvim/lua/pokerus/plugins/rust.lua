return {
  "simrat39/rust-tools.nvim",
  dependencies = {
    "mfussenegger/nvim-dap",
    "neovim/nvim-lspconfig",
    "nvim-lua/plenary.nvim",
  },
  ft = "rust",

  config = function()
    local opts = {
      server = {
        settings = {
          ["rust-analyzer"] = {
            check = {
              -- allTargets = false,
              -- extraArgs = { "--target", "thumbv6m-none-eabi" },
              command = "clippy",
            },
            -- cargo = { features = { "mlua" } },
            diagnostics = {
              disabled = { "unresolved-proc-macro" },
            },
          },
        },
        on_attach = require("pokerus.lsp").on_attach,
      },
    }

    -- Look for vscode-lldb extension
    local extension_path = vim.fn.globpath(vim.env.HOME .. "/.vscode/extensions", "vadimcn.vscode-lldb-*", false, true)
    if #extension_path > 0 then
      if #extension_path > 1 then
        vim.notify("Multiple versions of vscode-lldb appear to be installed!", vim.log.levels.WARN)
      end

      local codelldb_path = extension_path[1] .. 'adapter/codelldb'
      local liblldb_path = extension_path[1] .. 'lldb/lib/liblldb'
      local this_os = vim.loop.os_uname().sysname;

      -- The liblldb extension is .so for linux and .dylib for macOS
      liblldb_path = liblldb_path .. (this_os == "Linux" and ".so" or ".dylib")
      opts.dap = {
        adapter = require('rust-tools.dap').get_codelldb_adapter(codelldb_path, liblldb_path)
      }
    end

    require('rust-tools').setup(opts)
  end,
}
