local M = {}
M.opts = {
  settings = {
    ["rust-analyzer"] = {
      check = { 
        -- extraArgs = { "--target", "thumbv6m-none-eabi" },
        command = "clippy",
      },
      -- cargo = { features = { "mlua" } },
      diagnostics = {
        disabled = {
          "inactive-code",
          "unresolved-proc-macro",
        }, 
      },
    },
  },
}

return M
