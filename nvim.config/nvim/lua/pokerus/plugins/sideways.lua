return {
  plug = function(use)
    use {
      "AndrewRadev/sideways.vim",
      -- Move things sideways in lists
      config = function()
        require("pokerus").nmap {
          ["ch"] = { "<cmd>SidewaysLeft<cr>", "sideways-left" },
          ["cl"] = { "<cmd>SidewaysRight<cr>", "sideways-right" },
        }
      end,
    }
  end,
}
