local oil_detail = false

local function toggle_oil_detail()
  oil_detail = not oil_detail
  if oil_detail then
    require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
  else
    require("oil").set_columns({ "icon" })
  end
end

local function oil_grug()
  -- get the current directory
  local prefills = { paths = require("oil").get_current_dir() }

  local grug_far = require "grug-far"
  -- instance check
  if not grug_far.has_instance "explorer" then
    grug_far.open {
      instanceName = "explorer",
      prefills = prefills,
      staticTitle = "Find and Replace from Explorer",
    }
  else
    grug_far.open_instance "explorer"
    -- updating the prefills without clearing the search and other fields
    grug_far.update_instance_prefills("explorer", prefills, false)
  end
end

local function oil_split()
  local filename = vim.fn.expand("%:t")
  if not filename or filename == "" then
    if vim.api.nvim_get_option_value("filetype", {}) ~= "oil" then
      vim.notify("Cannot find parent directory: " .. filename, vim.log.levels.ERROR)
      return
    else
      -- We are in an oil buffer
      filename = vim.fn.expand("%")
      if filename == "oil:///" then
        vim.notify("Already at root directory", vim.log.levels.WARN)
        return
      end
      filename = string.sub(filename, string.find(filename, "[^/]*/$") or 0)
    end
  end

  vim.api.nvim_open_win(0, true, {
    split = "left",
  })

  require("oil").open()
  vim.fn.search(filename)
end

return {
  "stevearc/oil.nvim",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    -- Can also use "echasnovski/mini.icons" but I'm already using nvim-web-devicons elsewhere
    "MagicDuck/grug-far.nvim",
  },
  lazy = false,
  opts = {
    keymaps = {
      ["-"] = false,
      ["<C-p>"] = false,
      ["g-"] = "actions.parent",
      ["<C-s>"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
      ["<C-x>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
      ["K"] = { "actions.preview", mode = "n", desc = "Toggle preview" },
      ["gi"] = { callback = toggle_oil_detail, desc = "Toggle detailed file view" },
      ["g/"] = { callback = toggle_oil_detail, desc = "Search with grug" },
    },
    view_options = {
      show_hidden = true,
    }
  },
  keys = {
    { "g-", "<cmd>Oil<CR>", desc = "oil-open" },
    -- { "gl", open_oil("right"), desc = "oil-right" },
    { "gh", oil_split,      desc = "oil-split" },
  },
}
