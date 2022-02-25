return {
  plug = function(use)
    use {
      "junegunn/goyo.vim",
      config = function()
        vim.g.close_pair_key = 1

        require("pokerus").lmap { ["G"] = { "<cmd>Goyo<cr>", "close-pair" } }

        vim.cmd [[
          augroup goyo-settings
            autocmd!
            autocmd User GoyoEnter nested call g:ShareSetMode(1)
            autocmd User GoyoLeave nested call g:ShareSetMode(0)
          augroup END
        ]]
      end,
    }
  end,
}
