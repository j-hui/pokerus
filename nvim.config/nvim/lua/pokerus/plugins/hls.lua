vim.g["asterisk#keeppos"] = 1
return {
  { "haya14busa/vim-asterisk" },

  { "kevinhwang91/nvim-hlslens",
    dependencies = { "haya14busa/vim-asterisk" },
    opts = { nearest_only = true },
    keys = {
      { "n",
        [[<cmd>execute('normal! ' . v:count1 . 'n')<CR><cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "hls-next",
      },
      { "N",
        [[<cmd>execute('normal! ' . v:count1 . 'N')<CR><cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "hls-prev",
      },
      { "*",
        [[<Plug>(asterisk-z*)<cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "hls-cursor-next",
      },
      { "#",
        [[<Plug>(asterisk-z#)<cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "hls-cursor-prev",
      },
      { "g*",
        [[<Plug>(asterisk-g*)<cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "search-cursor-next",
      },
      { "g#",
        [[<Plug>(asterisk-g#)<cmd>lua require("hlslens").start()<CR>]],
        mode = "n",
        desc = "search-cursor-prev",
      },

      { "*",
        [[<Plug>(asterisk-z*)<cmd>lua require("hlslens").start()<CR>]],
        mode = "x",
        desc = "hls-cursor-next",
      },
      { "#",
        [[<Plug>(asterisk-z#)<cmd>lua require("hlslens").start()<CR>]],
        mode = "x",
        desc = "hls-cursor-prev",
      },
      { "g*",
        [[<Plug>(asterisk-g*)<cmd>lua require("hlslens").start()<CR>]],
        mode = "x",
        desc = "search-cursor-next",
      },
      { "g#",
        [[<Plug>(asterisk-g#)<cmd>lua require("hlslens").start()<CR>]],
        mode = "x",
        desc = "search-cursor-prev",
      }
    },
  }
}
