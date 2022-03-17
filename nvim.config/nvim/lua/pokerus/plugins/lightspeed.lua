return {
  plug = function(use)
    vim.g.lightspeed_no_default_keymaps = true
    use {
      "ggandor/lightspeed.nvim",
      opt = true,
      event = "VimEnter",
      config = function()
        require("lightspeed").setup {
          exit_after_idle_msecs = { labeled = 2500, unlabled = 1000 },
          limit_ft_matches = 16,
        }

        require("pokerus").nmap {
          ["f"] = { "<Plug>Lightspeed_f", "lightspeed-f" },
          ["F"] = { "<Plug>Lightspeed_F", "lightspeed-F" },
          ["t"] = { "<Plug>Lightspeed_t", "lightspeed-t" },
          ["T"] = { "<Plug>Lightspeed_T", "lightspeed-T" },
          ["s"] = { "<Plug>Lightspeed_s", "lightspeed-s" },
          ["S"] = { "<Plug>Lightspeed_S", "lightspeed-S" },
          ["g/"] = { "<Plug>Lightspeed_omni_s", "lightspeed-omni-s" },
          [","] = { "<Plug>Lightspeed_;_ft", "lightspeed-;" },
          ["[,"] = { "<Plug>Lightspeed_;_ft", "lightspeed-;" },
          ["],"] = { "<Plug>Lightspeed_,_ft", "lightspeed-;" },
        }

        require("pokerus").xmap {
          c = { "<Plug>Lightspeed_s", "lightspeed-s" },
          C = { "<Plug>Lightspeed_S", "lightspeed-S" },
        }

        require("pokerus").omap {
          c = { "<Plug>Lightspeed_s", "lightspeed-s" },
          C = { "<Plug>Lightspeed_S", "lightspeed-S" },
          x = { "<Plug>Lightspeed_x", "lightspeed-x" },
          X = { "<Plug>Lightspeed_X", "lightspeed-X" },
        }
      end,
    }
  end,
}
