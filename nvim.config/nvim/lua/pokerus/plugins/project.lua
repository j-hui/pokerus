return {
  "ahmedkhalf/project.nvim",
  opt = true,
  cmd = { "ProjectRoot", "Projects", "Proj" },
  dependencies = {
    "nvim-telescope/telescope.nvim",
    "neovim/nvim-lspconfig",
  },
  config = function()
    require("project_nvim").setup {
      silent_chdir = false,
    }
    require("telescope").load_extension "projects"
    vim.cmd [[command! Projects Telescope projects]]
    vim.cmd [[command! Proj Telescope projects]]
  end,
}
