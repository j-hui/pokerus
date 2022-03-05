local fn = vim.fn

local install_path = fn.stdpath "data" .. "/site/pack/packer/opt/packer.nvim"
local packer_bootstrap
if fn.empty(fn.glob(install_path)) > 0 then
  print "Installing packer"
  packer_bootstrap = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
end

vim.cmd [[packadd packer.nvim]]

return require("packer").startup {
  function(use)
    use { "wbthomason/packer.nvim", opt = true }
    require("pokerus.plugins.alpha").plug(use)
    require("pokerus.plugins.bufferline").plug(use)
    require("pokerus.plugins.close-pair").plug(use)
    require("pokerus.plugins.cmp").plug(use)
    require("pokerus.plugins.comment").plug(use)
    require("pokerus.plugins.coqtail").plug(use)
    require("pokerus.plugins.css-color").plug(use)
    require("pokerus.plugins.dressing").plug(use)
    require("pokerus.plugins.fidget").plug(use)
    require("pokerus.plugins.fugitive").plug(use)
    require("pokerus.plugins.gitlinker").plug(use)
    require("pokerus.plugins.gitsigns").plug(use)
    require("pokerus.plugins.goyo").plug(use)
    require("pokerus.plugins.hls").plug(use)
    require("pokerus.plugins.illuminate").plug(use)
    require("pokerus.plugins.interestingwords").plug(use)
    require("pokerus.plugins.latex").plug(use)
    require("pokerus.plugins.lightbulb").plug(use)
    require("pokerus.plugins.lightspeed").plug(use)
    require("pokerus.plugins.lir").plug(use)
    require("pokerus.plugins.lsp").plug(use)
    require("pokerus.plugins.lualine").plug(use)
    require("pokerus.plugins.markdown").plug(use)
    require("pokerus.plugins.marks").plug(use)
    require("pokerus.plugins.neogen").plug(use)
    require("pokerus.plugins.null-ls").plug(use)
    require("pokerus.plugins.numb").plug(use)
    require("pokerus.plugins.octo").plug(use)
    require("pokerus.plugins.project").plug(use)
    require("pokerus.plugins.scrollbar").plug(use)
    require("pokerus.plugins.sideways").plug(use)
    require("pokerus.plugins.stabilize").plug(use)
    require("pokerus.plugins.stickybuf").plug(use)
    require("pokerus.plugins.telescope").plug(use)
    require("pokerus.plugins.textobj").plug(use)
    require("pokerus.plugins.todo-comments").plug(use)
    require("pokerus.plugins.tokyonight").plug(use)
    require("pokerus.plugins.treesitter").plug(use)
    require("pokerus.plugins.trouble").plug(use)
    require("pokerus.plugins.twilight").plug(use)
    require("pokerus.plugins.venn").plug(use)
    require("pokerus.plugins.vimbridge").plug(use)
    require("pokerus.plugins.virt-column").plug(use)
    require("pokerus.plugins.vsnip").plug(use)
    require("pokerus.plugins.which-key").plug(use)
    require("pokerus.plugins.zk").plug(use)
    use "dstein64/vim-startuptime"

    if packer_bootstrap then
      -- Automatically set up configuration after cloning packer.nvim
      require("packer").sync()
    end
  end,
  config = {
    transitive_opt = false,
    profile = {
      enable = true,
      threshold = 5,
    },
  },
}
