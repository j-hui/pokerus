local M = {}

function M.init(typ, keywords, groups)
  vim.fn["css_color#init"](typ, keywords, table.concat(groups, ","))
end

function M.plug(use)
  use {
    "ap/vim-css-color",
    config = function()
      vim.cmd [[
        augroup CssColorCustomFiletypes
          autocmd!
          autocmd Filetype conf lua require("pokerus.plugins.css-color").init("hex", "none", {"confComment" , "confString"})
          autocmd Filetype conf lua require("pokerus.plugins.css-color").init("hex", "none", {"haskellLineComment" , "haskellString", haskellBlockComment})
        augroup END
      ]]
    end,
  }
end

return M
