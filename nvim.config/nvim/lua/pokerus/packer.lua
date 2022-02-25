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
    -- "Use" packer so that it doesn't get marked for deletion
    use { "wbthomason/packer.nvim", opt = true }

    -- Automatically require any plugin module in ~/.config/nvim/lua/pokerus/plugins/
    for _, p in ipairs(fn.split(fn.glob((fn.stdpath "config") .. "/lua/pokerus/plugins/*"))) do
      p = fn.substitute(p, "^.*/", "", "")
      p = fn.substitute(p, "\\.lua$", "", "")
      require("pokerus.plugins." .. p).plug(use)
    end

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
