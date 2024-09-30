local oil_detail = false

local function open_oil(dir)
  return function()
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

    if dir then
      vim.api.nvim_open_win(0, true, {
        split = dir,
      })
    end

    require("oil").open()
    vim.fn.search(filename)
  end
end

return {
  "stevearc/oil.nvim",
  dependencies = { "nvim-tree/nvim-web-devicons" }, -- Can also use "echasnovski/mini.icons" but I'm already using nvim-web-devicons elsewhere
  lazy = false,
  opts = {
    keymaps = {
      ["-"] = false,
      ["<C-p>"] = false,
      ["g-"] = "actions.parent",
      ["<C-s>"] = { "actions.select", opts = { vertical = true }, desc = "Open the entry in a vertical split" },
      ["<C-x>"] = { "actions.select", opts = { horizontal = true }, desc = "Open the entry in a horizontal split" },
      ["K"] = { "actions.preview", mode = "n", desc = "Toggle preview" },
      ["gi"] = {
        desc = "Toggle file detail view",
        callback = function()
          oil_detail = not oil_detail
          if oil_detail then
            require("oil").set_columns({ "icon", "permissions", "size", "mtime" })
          else
            require("oil").set_columns({ "icon" })
          end
        end,
      },
    },
    view_options = {
      show_hidden = true,
    }
  },
  keys = {
    { "g-", "<cmd>Oil<CR>",    desc = "oil" },
    { "gl", open_oil("right"), desc = "oil-right" },
    { "gh", open_oil("left"),  desc = "oil-left" },
  },
}
