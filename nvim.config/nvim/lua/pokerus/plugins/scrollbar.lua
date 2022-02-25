return {
  plug = function(use)
    use {
      "petertriho/nvim-scrollbar",
      config = function()
        require("scrollbar").setup {}
        vim.cmd [[
          augroup scrollbar-highlights
            autocmd!
            autocmd Colorscheme * highlight! link ScrollbarHandle StatusLine
          augroup END
        ]]
      end,
    }
  end,
}
