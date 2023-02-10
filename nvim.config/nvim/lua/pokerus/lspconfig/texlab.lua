local fwdsearch = nil

if vim.fn.has("mac") then
  local skim_path = "/Applications/Skim.app/Contents/SharedSupport/displayline"

  if vim.fn.executable(skim_path) ~= 0 then
    fwdsearch = {
      executable = skim_path,
      args = { "-g", "%l", "%p", "%f" },
    }
    -- NOTE: to enable inverse-search:
    -- https://github.com/f3fora/nvim-texlabconfig/#skim
  else
    fwdsearch = {
      executable = "open",
      args = { "%p" },
    }
  end

elseif vim.fn.has("linux") then
  if vim.fn.executable("zathura") then
    fwdsearch = {
      executable = "zathura",
      args = vim.g.use_texlabconfig
          and {
            "--synctex-editor-command",
            [[nvim-texlabconfig -file '%{input}' -line %{line}]],
            "--synctex-forward", "%l:1:%f", "%p"
          }
          or {
            "--synctex-forward", "%l:1:%f", "%p"
          },
    }
  end
end

return {
  on_attach = function(client, bufnr)
    require("pokerus.lsp").on_attach(client, bufnr)
    vim.keymap.set("n", "<leader>lc", "<cmd>TexlabBuild<CR>", { desc = "tex-build" })
    if fwdsearch then
      vim.keymap.set("n", "<leader>lf", "<cmd>TexlabForward<CR>", { desc = "tex-goto-line" })
    end
  end,
  settings = {
    texlab = {
      build = { onSave = true },
      chkTex = { onOpenAndSave = true },
      forwardSearch = fwdsearch,
    },
  },
}
