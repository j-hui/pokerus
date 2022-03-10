local M = {}

function M.plug(use)
  -- No setup {{{
  use "tpope/vim-unimpaired"
  -- ]* and [* mappings

  use "tpope/vim-endwise"
  -- Automatically add closing token where appropriate

  use "tpope/vim-abolish"
  -- Smarter subtitutions

  use "tpope/vim-eunuch"
  -- UNIX-like functionality in Vim

  use "tommcdo/vim-exchange"
  -- Exchange text with repeated cx{motion}

  use "duggiefresh/vim-easydir"
  -- Create directories when non-existent

  use "lervag/file-line"
  -- Open file:line

  use "andymass/vim-matchup"
  -- %-navigate user-defined pairs

  use "christoomey/vim-titlecase"
  -- Title case w/ gz<motion>

  use "cosminadrianpopescu/vim-tail"
  -- Make vim behave like tail -f

  use "guns/xterm-color-table.vim"
  -- Preview all 256 xterm colors

  use "moll/vim-bbye"
  -- Delete buffers without messing up buffer layout

  use "AndrewRadev/bufferize.vim"
  -- Command contents in buffer

  use "AndrewRadev/linediff.vim"
  -- Vimdiff line ranges

  -- No setup }}}

  -- Vim setup {{{
  use {
    "lambdalisue/suda.vim",
    -- Give vim sudo powers
    config = function()
      require("pokerus").vimsetup "suda"
    end,
  }

  use {
    "svermeulen/vim-subversive",
    -- Substitute from yank
    config = function()
      require("pokerus").vimsetup "subversive"
    end,
  }

  use {
    "svermeulen/vim-cutlass",
    -- x and D only delete, no yank/cut but retain cut behavior for d
    config = function()
      require("pokerus").vimsetup "cutlass"
    end,
  }

  use {
    "machakann/vim-sandwich",
    -- Fancier vim-surround + d
    config = function()
      require("pokerus").vimsetup "sandwich"
    end,
  }

  use {
    "ojroques/vim-oscyank",
    -- Yank across the terminal
    config = function()
      require("pokerus").vimsetup "oscyank"
    end,
  }

  use {
    "junegunn/fzf",
    -- The classic fuzzy-finder
    run = function()
      vim.fn["fzf#install"]()
    end,
    config = function()
      require("pokerus").vimsetup "fzf"
    end,
  }
  -- Vim setup }}}

  -- Filetypes {{{
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
  use "andy-morris/happy.vim"
  use "andy-morris/alex.vim"
  use "euclidianAce/BetterLua.vim"
  -- Filetypes }}}

  -- Filetypes w/ setup {{{
  use {
    "fatih/vim-go",
    config = function()
      require("pokerus").vimsetup "go"
    end,
  }

  use {
    "octol/vim-cpp-enhanced-highlight",
    config = function()
      require("pokerus").vimsetup "cpp_enhanced"
    end,
  }

  use {
    "neovimhaskell/haskell-vim",
    config = function()
      require("pokerus").vimsetup "haskell"
    end,
  }

  use {
    "ziglang/zig.vim",
    config = function()
      require("pokerus").vimsetup "zig"
    end,
  }
  -- Filetypes w/ setup }}}

  -- Filetypes configured elsewhere:
  -- markdown
  -- coq
end
return M
