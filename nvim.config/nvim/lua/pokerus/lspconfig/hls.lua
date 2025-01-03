return {
  -- delegate to haskell-tools
  lspconfig = false,
  opts = {
    settings = {
      haskell = {
        formattingProvider = "fourmolu",
        plugin = {
          stan = {
            globalOn = false,
          },
        },
      },
    }, 
  },
}
