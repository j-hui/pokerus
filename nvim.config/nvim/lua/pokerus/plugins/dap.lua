return {
  "mfussenegger/nvim-dap",
  dependencies = {
    { "rcarriga/nvim-dap-ui",            config = true },
    { "theHamsta/nvim-dap-virtual-text", config = true },
  },
  keys = {
    { "<leader>d<CR>", function() require('dapui').toggle() end,           mode = "n",        desc = "dap-ui" },
    { "<leader>dc",    function() require('dap').continue() end,           mode = "n",        desc = "dap-continue" },
    { "<leader>dn",    function() require('dap').step_over() end,          mode = "n",        desc = "dap-next" },
    { "<leader>di",    function() require('dap').step_into() end,          mode = "n",        desc = "dap-step-in" },
    { "<leader>do",    function() require('dap').step_out() end,           mode = "n",        desc = "dap-step-out" },
    { "<leader>db",    function() require('dap').toggle_breakpoint() end,  mode = "n",        desc = "dap-breakpoint" },
    { "<leader>dr",    function() require('dap').repl.open() end,          mode = "n",        desc = "dap-repl" },
    { "<leader>dl",    function() require('dap').run_last() end,           mode = "n",        desc = "dap-last" },
    { "<leader>dk",    function() require('dap.ui.widgets').hover() end,   mode = { "n", "v" }, desc = "dap-hover" },
    { "<leader>dj",    function() require('dap.ui.widgets').preview() end, mode = { "n", "v" }, desc = "dap-preview" },
  },
}
