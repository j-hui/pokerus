local fn = vim.fn

local install_path = fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
local packer_bootstrap
if fn.empty(fn.glob(install_path)) > 0 then
  packer_bootstrap = fn.system {
    "git",
    "clone",
    "--depth",
    "1",
    "https://github.com/wbthomason/packer.nvim",
    install_path,
  }
end

return require("packer").startup(function(use)
  require "pokerus.plugins.vimbridge"(use)
  require "pokerus.plugins.comment"(use)

  use "leanprover/lean.vim"
  use "idris-hackers/idris-vim"
  use "LnL7/vim-nix"
  use "baskerville/vim-sxhkdrc"
  use "blyoa/vim-promela-syntax"
  use "chrisbra/csv.vim"
  use "rust-lang/rust.vim"
  use "leafgarland/typescript-vim"
  use "keith/swift.vim"
  use "dag/vim-fish"
  use "cespare/vim-toml"
  use "neomutt/neomutt.vim"
  use "liuchengxu/graphviz.vim"
  use "editorconfig/editorconfig-vim"
  use "mboughaba/i3config.vim"
  use "vim-scripts/DoxygenToolkit.vim"
  use "fatih/vim-go"
  use "octol/vim-cpp-enhanced-highlight"
  -- Better highlighting for C/C++
  use "neovimhaskell/haskell-vim"
  use "andy-morris/happy.vim"
  use "andy-morris/alex.vim"

  -- use 'preservim/vim-markdown', { 'as': 'preservim-vim-markdown' }

  use "whonore/coqtail"

  if packer_bootstrap then
    -- Automatically set up configuration after cloning packer.nvim
    require("packer").sync()
  end
end)
