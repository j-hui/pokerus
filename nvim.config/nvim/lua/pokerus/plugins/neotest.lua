return {
  "nvim-neotest/neotest",
  dependencies = {
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "antoinemadec/FixCursorHold.nvim",
    "nvim-treesitter/nvim-treesitter",
    "mfussenegger/nvim-dap",

    "nvim-neotest/neotest-vim-test",
    "nvim-neotest/neotest-plenary",
    "folke/neodev.nvim",
    "rouge8/neotest-rust",
    "mrcjkb/neotest-haskell",
  },
  event = "VeryLazy",
  keys = {
    { "<leader>x<space>", "<cmd>Neotest summary<CR>",      mode = "n", desc = "neotest-summary" },
    { "<leader>xx",       "<cmd>Neotest run<CR>",          mode = "n", desc = "neotest-run" },
    { "<leader>xX",       "<cmd>Neotest run file<CR>",     mode = "n", desc = "neotest-run-file" },
    { "<leader>xq",       "<cmd>Neotest stop<CR>",         mode = "n", desc = "neotest-stop" },
    { "<leader>xk",       "<cmd>Neotest output<CR>",       mode = "n", desc = "neotest-output" },
    { "<leader>xo",       "<cmd>Neotest output-panel<CR>", mode = "n", desc = "neotest-output-panel" },
    { "<leader>xo",       "<cmd>Neotest output-panel<CR>", mode = "n", desc = "neotest-output-panel" },
    { "]x",               "<cmd>Neotest jump next<CR>",    mode = "n", desc = "neotest-next" },
    { "[x",               "<cmd>Neotest jump prev<CR>",    mode = "n", desc = "neotest-prev" },
    {
      "<leader>xd",
      function()
        require("neotest").run.run({ strategy = "dap" })
      end,
      mode = "n",
      desc = "neotest-debug"
    },
  },
  config = function()
    require("neotest").setup({
      adapters = {
        require("neotest-plenary"),
        require("neotest-rust") {
          args = { "--no-capture" },
          dap_adapter = "lldb",
        },
        require("neotest-haskell"),
        require("neotest-vim-test")({
          ignore_file_types = { "python", "vim", "lua" },
        }),
      }
    })
  end,
}
