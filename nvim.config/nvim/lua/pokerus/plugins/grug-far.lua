return {
  "MagicDuck/grug-far.nvim",
  main = "grug-far", -- i.e., require(main), because Lazy has trouble inferring this
  opts = {
    keymaps = {
      -- I don't want to set '<localleader>' to 'g' but am otherwise happy with the bindings.
      replace = { n = "gr" },
      qflist = { n = "gq" },
      syncLocations = { n = "gs" },
      syncLine = { n = "gl" },
      close = { n = "gc" },
      historyOpen = { n = "gt" },
      historyAdd = { n = "ga" },
      refresh = { n = "gf" },
      openLocation = { n = "go" },
      openNextLocation = { n = "<down>" },
      openPrevLocation = { n = "<up>" },
      gotoLocation = { n = "<enter>" },
      pickHistoryEntry = { n = "<enter>" },
      abort = { n = "gb" },
      help = { n = "g?" },
      toggleShowCommand = { n = "gp" },
      swapEngine = { n = "ge" },
      previewLocation = { n = "gi" },
      swapReplacementInterpreter = { n = "gx" },
    }
  },
}
