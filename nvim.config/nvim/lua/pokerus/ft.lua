local M = {}

M.filetypes = {
  filename = {
    justfile = "just",
    [".envrc"] = "sh",
  },
  extension = {
    h = "cpp",
    v = "coq",
    x = "alex",
    y = "happy",
    pio = "pio",
    luau = "luau",
    rbxlx = "xml",
    pbxproj = "pbxproj",
    modulemap = "modulemap",
  },
}


function M.setup()
  vim.filetype.add(M.filetypes)

  pcall(function()
    ---@diagnostic disable-next-line: duplicate-set-field
    require("editorconfig").properties.trim_trailing_whitespace = function()
      -- disable this specific behavior. it screws up my diffs.
    end
  end)
end

return M
