local function setup_lldb()
  -- find lldb-vscode path challenge
  local lldb_path
  local sysname = vim.loop.os_uname().sysname
  if sysname == "Darwin" then
    -- NOTE: Install lldb using `brew install llvm`
    if vim.fn.system("which brew") == "" then
      -- no homebrew
      return
    end

    lldb_path = vim.trim(vim.fn.system("brew --prefix llvm")) .. "/bin/lldb-vscode"
    if vim.fn.system("which " .. lldb_path) == "" then
      -- lldb-vscode not present
      return
    end
    -- elseif sysname == "Linux" then
  else
    return
  end

  local dap = require('dap')

  dap.adapters.lldb = {
    type = "executable",
    command = lldb_path,
    name = "lldb"
  }

  dap.configurations.cpp = {
    {
      name = "Launch",
      type = "lldb",
      request = "launch",
      program = "/Users/j-hui/Documents/research/lice/target/debug/lice",
      -- program = function()
      --   local co = coroutine.running()
      --   assert(co, "must be running under a coroutine")
      --
      --   vim.ui.input({
      --     prompt = "Path to executable: ",
      --     default = vim.fn.getcwd() .. "/",
      --     completion = "file",
      --   }, function(input)
      --     coroutine.resume(co, input)
      --   end)
      --
      --   return coroutine.yield()
      --   -- return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
      -- end,
      cwd = "${workspaceFolder}",
      stopOnEntry = false,
      args = function()
        return { vim.fn.input("Arg: ") }
      end,
    },
  }

  dap.configurations.c = dap.configurations.cpp
  dap.configurations.rust = { vim.tbl_extend("force", dap.configurations.cpp[1], {
    initCommands = function()
      -- Find out where to look for the pretty printer Python module
      local rustc_sysroot = vim.fn.trim(vim.fn.system("rustc --print sysroot"))

      local script_import = 'command script import "' .. rustc_sysroot .. '/lib/rustlib/etc/lldb_lookup.py"'
      local commands_file = rustc_sysroot .. "/lib/rustlib/etc/lldb_commands"

      local commands = {}
      local file = io.open(commands_file, "r")
      if file then
        for line in file:lines() do
          table.insert(commands, line)
        end
        file:close()
      end
      table.insert(commands, 1, script_import)

      return commands
    end,
  }) }
end

return {
  "mfussenegger/nvim-dap",
  dependencies = {
    { "rcarriga/nvim-dap-ui",            config = true },
    { "theHamsta/nvim-dap-virtual-text", config = true },
    "nvim-telescope/telescope.nvim",
    "nvim-telescope/telescope-dap.nvim",
  },
  keys = {
    { "<leader>d<CR>", function() require('dapui').toggle() end,           mode = "n",          desc = "dap-ui" },
    { "<leader>dc",    function() require('dap').continue() end,           mode = "n",          desc = "dap-continue" },
    { "<leader>dn",    function() require('dap').step_over() end,          mode = "n",          desc = "dap-next" },
    { "<leader>di",    function() require('dap').step_into() end,          mode = "n",          desc = "dap-step-in" },
    { "<leader>do",    function() require('dap').step_out() end,           mode = "n",          desc = "dap-step-out" },
    { "<leader>db",    function() require('dap').toggle_breakpoint() end,  mode = "n",          desc = "dap-breakpoint" },
    { "<leader>dr",    function() require('dap').repl.open() end,          mode = "n",          desc = "dap-repl" },
    { "<leader>dl",    function() require('dap').run_last() end,           mode = "n",          desc = "dap-last" },
    { "<leader>dq",    function() require('dap').terminate() end,          mode = "n",          desc = "dap-quit" },
    { "<leader>dk",    function() require('dap.ui.widgets').hover() end,   mode = { "n", "v" }, desc = "dap-hover" },
    { "<leader>dj",    function() require('dap.ui.widgets').preview() end, mode = { "n", "v" }, desc = "dap-preview" },
  },
  config = function()
    setup_lldb()
    vim.api.nvim_create_autocmd("Filetype", {
      pattern = "dap-float",
      callback = function()
        vim.keymap.set("n", "q", "<cmd>quit<cr>", { desc = "close-float" })
      end
    })
    require("telescope").load_extension("dap")
  end,
}
