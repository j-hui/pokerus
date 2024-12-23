local M = {
  "mfussenegger/nvim-dap",
  dependencies = {
    { "rcarriga/nvim-dap-ui",            config = true },
    { "theHamsta/nvim-dap-virtual-text", config = true },
    -- "nvim-telescope/telescope.nvim",
    -- "nvim-telescope/telescope-dap.nvim",
  },
  keys = {
    { "<leader>d<CR>", function() require("dapui").toggle() end,           mode = "n",          desc = "dap-ui" },
    { "<leader>dc",    function() require("dap").continue() end,           mode = "n",          desc = "dap-continue" },
    { "<leader>dn",    function() require("dap").step_over() end,          mode = "n",          desc = "dap-next" },
    { "<leader>di",    function() require("dap").step_into() end,          mode = "n",          desc = "dap-step-in" },
    { "<leader>do",    function() require("dap").step_out() end,           mode = "n",          desc = "dap-step-out" },
    { "<leader>db",    function() require("dap").toggle_breakpoint() end,  mode = "n",          desc = "dap-breakpoint" },
    { "<leader>dr",    function() require("dap").repl.open() end,          mode = "n",          desc = "dap-repl" },
    { "<leader>dl",    function() require("dap").run_last() end,           mode = "n",          desc = "dap-last" },
    { "<leader>dq",    function() require("dap").terminate() end,          mode = "n",          desc = "dap-quit" },
    { "<leader>dk",    function() require("dap.ui.widgets").hover() end,   mode = { "n", "v" }, desc = "dap-hover" },
    { "<leader>dj",    function() require("dap.ui.widgets").preview() end, mode = { "n", "v" }, desc = "dap-preview" },
  }
}

function M.dap_up()
  local function notify(msg, level)
    vim.notify(msg, level or vim.log.levels.INFO, {
      annote = "nvim-dap",
      group = "nvim-dap",
    })
  end

  local file = vim.api.nvim_buf_get_name(0)
  for dir in vim.fs.parents(file) do
    local names = { "nvim-dap-config.lua", ".nvim-dap-config.lua" }
    for _, name in ipairs(names) do
      local path = dir .. "/" .. name
      if vim.fn.filereadable(path) == 1 then
        notify("Found configuration: " .. path)
        dofile(path)
        notify("Ready to debug: " .. file)
        require("dapui").open()
        return
      end
    end
  end
  notify("Could not find configuration for file: " .. file, vim.log.levels.ERROR)
end

table.insert(M.keys, { "<leader>du", M.dap_up, mode = "n", desc = "dap-up" })

function M.lldb_dap_path()
  -- Easiest: just use whatever is on PATH
  if vim.fn.executable("lldb-dap") then
    return "lldb-dap"
  end

  local sysname = vim.loop.os_uname().sysname
  if sysname == "Darwin" then
    -- Use brew-installed lldb-dap (might not be on PATH)
    if vim.fn.executable("brew") == 1 then
      local path = vim.trim(vim.fn.system("brew --prefix llvm")) .. "/bin/lldb-dap"
      if vim.fn.filereadable(path) == 1 then
        return path
      end
    end

    -- Use xcode-bundled lldb-dap (but doing this is slow, so disabled for now)
    if false and vim.fn.executable("xcrun") == 1 then
      local path = vim.trim(vim.fn.system("xcrun -f lldb-dap"))
      if vim.fn.filereadable(path) == 1 then
        return path
      end
    end
  end

  -- Cannot find it
  return nil
end

-- Pass this function to dap.configurations.rust.<config>.initCommands
function M.init_rust()
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
end

function M.config()
  local lldb_dap = M.lldb_dap_path()
  if lldb_dap ~= nil then
    require("dap").adapters.lldb = {
      type = "executable",
      command = lldb_dap,
      name = "lldb"
    }
  end

  vim.api.nvim_create_user_command("DapUp", M.dap_up, {})

  vim.api.nvim_create_autocmd("Filetype", {
    pattern = "dap-float",
    callback = function()
      vim.keymap.set("n", "q", "<cmd>quit<cr>", { desc = "close-float" })
    end
  })

  -- require("telescope").load_extension("dap")
end

return {}
-- return M
