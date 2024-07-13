return false or {
  settings = {
    ["rust-analyzer"] = {
      check = {
        -- allTargets = false,
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
  }
}
