return {
  plug = function(use)
    use {
      "nvim-lualine/lualine.nvim",
      after = { "theme" },
      requires = {
        "kyazdani42/nvim-web-devicons",
        "SmiteshP/nvim-gps",
        "nvim-treesitter/nvim-treesitter",
      },
      config = function()
        local gps = require "nvim-gps"
        gps.setup {}

        require("lualine").setup {
          options = {
            section_separators = "",
            component_separators = "",
          },
          sections = {
            lualine_b = {
              "branch",
              "diff",
              { "diagnostics", colored = false },
            },
            lualine_c = {
              "hostname",
              "filename",
              { gps.get_location, cond = gps.is_available },
            },
            lualine_x = {
              function()
                if
                  vim.bo.filetype ~= "md"
                  and vim.bo.filetype ~= "txt"
                  and vim.bo.filetype ~= "text"
                  and vim.bo.filetype ~= "markdown"
                then
                  return ""
                end
                local wc = vim.fn.wordcount()
                local count = wc.visual_words and wc.visual_words or wc.words
                if count == 1 then
                  return tostring(count) .. " word"
                else
                  return tostring(count) .. " words"
                end
              end,
              "filetype",
              "encoding",
              "fileformat",
            },
          },
          extensions = {
            "fugitive",
            "fzf",
            "fern",
          },
        }
      end,
    }
  end,
}
