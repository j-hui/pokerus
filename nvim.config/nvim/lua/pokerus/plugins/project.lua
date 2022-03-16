return {
  plug = function(use)
    use {
      "ahmedkhalf/project.nvim",
      opt = true,
      cmd = { "ProjectRoot", "Projects" },
      requires = { "nvim-telescope/telescope.nvim" },
      config = function()
        require("project_nvim").setup {
          manual_mode = true,
          silent_chdir = false,
        }
        require("telescope").load_extension "projects"
        vim.cmd [[command! Projects Telescope projects]]
      end,
    }
  end,
}
