return {
  on_attach = function(client, bufnr)
    require("pokerus.lsp").on_attach(client, bufnr)
    vim.keymap.set("n", "<leader>lc", "<cmd>TexlabBuild<CR>", { desc = "tex-build" })
    vim.keymap.set("n", "<leader>lf", "<cmd>TexlabForward<CR>", { desc = "tex-goto-line" })
  end,
  settings = {
    texlab = {
      build = { onSave = true },
      chkTex = { onOpenAndSave = true },
      forwardSearch = (function()
        local skim_path = "/Applications/Skim.app/Contents/SharedSupport/displayline"
        if vim.fn.executable(skim_path) ~= 0 then
          return {
            executable = skim_path,
            args = { "-g", "%l", "%p", "%f" },
          }
        else
          return {
            executable = "zathura",
            args = { "--synctex-forward", "%l:1:%f", "%p" },
          }
        end
      end)()
    },
  },
}
