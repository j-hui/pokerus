return {
  plug = function(use)
    use {
      "ruifm/gitlinker.nvim",
      requires = { "nvim-lua/plenary.nvim", "ojroques/vim-oscyank" },
      config = function()
        require("gitlinker").setup {
          opts = {
            mappings = "<leader>gy",
            action_callback = function(url)
              vim.api.nvim_command("let @\" = '" .. url .. "'")
              vim.fn.OSCYankString(url)
            end,
          },
        }
        require("pokerus").nmap { ["<leader>gy"] = "git-yank-url" }
      end,
    }
  end,
}
