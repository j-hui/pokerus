return {
  "nvim-neotest/neotest",
  dependencies = {
    -- Core dependencies
    "nvim-neotest/nvim-nio",
    "nvim-lua/plenary.nvim",
    "nvim-treesitter/nvim-treesitter",

    -- Extras
    "mfussenegger/nvim-dap",
    "antoinemadec/FixCursorHold.nvim",

    -- Adapters
    "nvim-neotest/neotest-vim-test",
    "nvim-neotest/neotest-plenary",
    "alfaix/neotest-gtest", -- C++ gtest
    "rouge8/neotest-rust",  -- Treesitter-based testing
    -- "mrcjkb/neotest-haskell", -- takes a long time to build
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
        require("neotest-vim-test")({
          ignore_file_types = { "python", "vim", "lua" },
        }),
        require("neotest-rust") {
          args = { "--no-capture" },
          dap_adapter = "lldb",
        },
        -- require("neotest-haskell"),
        require("neotest-gtest").setup({
          debug_adapter = "lldb",
        }),
      }
    })
  end,
}
