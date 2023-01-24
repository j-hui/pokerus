local function harpoon(m, f)
  return function()
    require("harpoon." .. m)[f]()
  end
end

return {
  "ThePrimeagen/harpoon",
  dependencies = {
    "nvim-lua/plenary.nvim",
    "nvim-telescope/telescope.nvim",
  },
  keys = {
    { "<leader>m", harpoon("mark", "add_file"), mode = "n", desc = "harpoon-mark" },
    { "<leader>'", "<cmd>Telescope harpoon marks<CR>", mode = "n", desc = "harpoon-jump" },
    { "<leader>`", harpoon("ui", "toggle_quick_menu"), mode = "n", desc = "harpoon-menu" },
    { "[m", harpoon("ui", "nav_prev"), mode = "n", desc = "harpoon-prev" },
    { "]m", harpoon("ui", "nav_next"), mode = "n", desc = "harpoon-next" },
  },
  config = function()
    require("telescope").load_extension "harpoon"
  end,
}
