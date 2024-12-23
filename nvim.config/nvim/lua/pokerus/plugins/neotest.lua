---@diagnostic disable: missing-fields
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
    { "<leader>tt", "<cmd>Neotest summary<CR>",      desc = "neotest-summary" },
    { "<leader>tr", "<cmd>Neotest run<CR>",          desc = "neotest-run" },
    { "<leader>t.", "<cmd>Neotest run file<CR>",     desc = "neotest-run-file" },
    { "<leader>tq", "<cmd>Neotest stop<CR>",         desc = "neotest-stop" },
    { "<leader>tk", "<cmd>Neotest output<CR>",       desc = "neotest-output" },
    { "<leader>to", "<cmd>Neotest output-panel<CR>", desc = "neotest-output-panel" },
    { "]T",         "<cmd>Neotest jump next<CR>",    desc = "neotest-next" },
    { "[T",         "<cmd>Neotest jump prev<CR>",    desc = "neotest-prev" },
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
