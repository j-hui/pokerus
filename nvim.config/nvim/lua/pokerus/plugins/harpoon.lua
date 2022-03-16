local M = {}

function M.plug(use)
  use {
    "ThePrimeagen/harpoon",
    requires = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope.nvim",
    },
    opt = true,
    event = { "VimEnter" },
    config = function()
      require("telescope").load_extension "harpoon"

      require("pokerus").nmap {
        ["<leader>m"] = {
          function()
            require("harpoon.mark").add_file()
          end,
          "harpoon-mark",
        },
        ["<leader>'"] = { "<cmd>Telescope harpoon marks<CR>", "harpoon-jump" },
        ["<leader>`"] = {
          function()
            require("harpoon.ui").toggle_quick_menu()
          end,
          "harpoon-menu",
        },
        ["[m"] = {
          function()
            require("harpoon.ui").nav_prev()
          end,
          "harpoon-prev",
        },
        ["]m"] = {
          function()
            require("harpoon.ui").nav_next()
          end,
          "harpoon-next",
        },
      }
    end,
  }
end

return M
