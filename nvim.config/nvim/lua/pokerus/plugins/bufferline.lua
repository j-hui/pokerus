return {
  plug = function(use)
    use {
      "akinsho/bufferline.nvim",
      requires = {
        "kyazdani42/nvim-web-devicons",
      },
      config = function()
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
          ["[b"] = { "<cmd>BufferLineCyclePrev", "buffer-prev" },
          ["]b"] = { "<cmd>BufferLineCycleNext", "buffer-next" },
        }
      end,
    }
  end,
}
