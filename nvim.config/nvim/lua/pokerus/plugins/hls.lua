return {
  plug = function(use)
    use {
      "haya14busa/vim-asterisk",
      config = function()
        vim.g["asterisk#keeppos"] = 1
      end,
    }

    use {
      "kevinhwang91/nvim-hlslens",
      requires = { "haya14busa/vim-asterisk" },
      config = function()
        require("hlslens").setup { nearest_only = true }

        require("pokerus").nmap({
          n = {
            [[<cmd>execute('normal! ' . v:count1 . 'n')<CR><cmd>lua require("hlslens").start()<CR>]],
            "hls-next",
          },
          N = {
            [[<cmd>execute('normal! ' . v:count1 . 'N')<CR><cmd>lua require("hlslens").start()<CR>]],
            "hls-prev",
          },
          ["*"] = {
            [[<Plug>(asterisk-z*)<cmd>lua require("hlslens").start()<CR>]],
            "hls-cursor-next",
          },
          ["#"] = {
            [[<Plug>(asterisk-z#)<cmd>lua require("hlslens").start()<CR>]],
            "hls-cursor-prev",
          },
          ["g*"] = {
            [[<Plug>(asterisk-g*)<cmd>lua require("hlslens").start()<CR>]],
            "search-cursor-next",
          },
          ["g#"] = {
            [[<Plug>(asterisk-g#)<cmd>lua require("hlslens").start()<CR>]],
            "search-cursor-prev",
          },
        }, { noremap = true, silent = true })

        require("pokerus").xmap({
          ["*"] = {
            [[<Plug>(asterisk-z*)<cmd>lua require("hlslens").start()<CR>]],
            "hls-cursor-next",
          },
          ["#"] = {
            [[<Plug>(asterisk-z#)<cmd>lua require("hlslens").start()<CR>]],
            "hls-cursor-prev",
          },
          ["g*"] = {
            [[<Plug>(asterisk-g*)<cmd>lua require("hlslens").start()<CR>]],
            "search-cursor-next",
          },
          ["g#"] = {
            [[<Plug>(asterisk-g#)<cmd>lua require("hlslens").start()<CR>]],
            "search-cursor-prev",
          },
        }, { noremap = true, silent = true })
      end,
    }
  end,
}
