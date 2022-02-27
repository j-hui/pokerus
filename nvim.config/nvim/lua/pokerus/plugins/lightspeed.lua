return {
  plug = function(use)
    use {
      "ggandor/lightspeed.nvim",
      config = function()
        require("lightspeed").setup {
          exit_after_idle_msecs = { labeled = 2500, unlabled = 1000 },
          limit_ft_matches = 16,
        }
        require("pokerus").xmap {
          s = "<Plug>Lightspeed_s",
          S = "<Plug>Lightspeed_S",
        }
        require("pokerus").omap {
          c = "<Plug>Lightspeed_s",
          C = "<Plug>Lightspeed_S",
        }
        require("pokerus").nmap {
          [","] = "<Plug>Lightspeed_;_ft",
          ["[,"] = "<Plug>Lightspeed_;_ft",
          ["],"] = "<Plug>Lightspeed_,_ft",
        }
      end,
    }
  end,
}
