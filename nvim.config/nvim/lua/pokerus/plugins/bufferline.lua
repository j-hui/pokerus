local M = {}

function M.config()
  require("bufferline").setup {
    options = {
      diagnostics = "nvim_lsp",
      numbers = "buffer_id",
      diagnostics_indicator = function(count, level) -- , diagnostics_dict, context)
        if level:match "error" then
          return "  " .. count
        end
        return ""
      end,
    },
  }
  require("pokerus").nmap {
    ["[b"] = { "<cmd>BufferLineCyclePrev<CR>", "buffer-prev" },
    ["]b"] = { "<cmd>BufferLineCycleNext<CR>", "buffer-next" },
  }
end

function M.plug(use)
  use {
    "akinsho/bufferline.nvim",
    after = { "theme" },
    requires = {
      "kyazdani42/nvim-web-devicons",
    },
    config = function()
      require("pokerus.plugins.bufferline").config()
    end,
  }
end

return M
