return {
  plug = function(use)
    use {
      "hrsh7th/vim-vsnip",
      requires = { "rafamadriz/friendly-snippets" },
      config = function()
        vim.cmd [[
          imap <expr> <C-l> vsnip#available(1) ? '<Plug>(vsnip-expand-or-jump)' : '\<C-y>'
          smap <expr> <C-l> vsnip#available(2) ? '<Plug>(vsnip-expand-or-jump)' : '\<C-y>'
        ]]
      end,
    }
  end,
}
