local M = {}

function M.config()
  require("spectre").setup()

  require("pokerus").nmap({
    name = "spectre",
    s = {
      function()
        require("spectre").open()
      end,
      "spectre-open",
    },
    w = {
      function()
        require("spectre").open_visual { select_word = true }
      end,
      "spectre-word",
    },
    p = {
      function()
        require("spectre").open_file_search()
      end,
      "spectre-word",
    },
  }, { prefix = "<leader>s" })

  require("pokerus").vmap {
    ["<leader>s"] = {
      function()
        require("spectre").open_visual()
      end,
      "spectre",
    },
  }
end

function M.plug(use)
  use {
    "windwp/nvim-spectre",
    requires = { "nvim-lua/plenary.nvim" },
    opt = true,
    event = "VimEnter",
    config = function()
      require("pokerus.plugins.spectre").config()
    end,
  }
end
return M
