local function wordcount()
  if vim.bo.filetype ~= "md"
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
end

return {
  "nvim-lualine/lualine.nvim",
  event = "UIEnter",
  dependencies = {
    "nvim-tree/nvim-web-devicons",
    "nvim-treesitter/nvim-treesitter",
  },
  config = function()
    require("lualine").setup {
      options = {
        section_separators = "",
        component_separators = "",
      },
      sections = {
        lualine_a = { "mode" },
        lualine_b = {
          "branch",
          "diff",
          { "diagnostics", colored = false },
        },
        lualine_c = {
          -- "hostname",
          "filename",
        },
        lualine_x = {
          wordcount,
          "filetype",
          "encoding",
          "fileformat",
        },
        lualine_y = { "progress" },
        lualine_z = { "location" },
      },
      extensions = {
        "fugitive",
        "fzf",
      },
    }
  end,
}
