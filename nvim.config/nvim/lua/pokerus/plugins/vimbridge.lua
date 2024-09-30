return {
  -- No setup {{{
  { "tpope/vim-unimpaired", event = "VeryLazy" },
  -- ]* and [* mappings

  { "tpope/vim-abolish",
    event = "VeryLazy",
    config = function()
      require("pokerus").vimsetup "abolish"
    end,
  },
  -- Smarter subtitutions

  { "tpope/vim-eunuch", event = "InsertEnter" },
  -- UNIX-like functionality in Vim

  { "tommcdo/vim-exchange", event = "VeryLazy" },
  -- Exchange text with repeated cx{motion}

  { "duggiefresh/vim-easydir" },
  -- Create directories when non-existent

  { "lervag/file-line" },
  -- Open file:line

  { "christoomey/vim-titlecase", event = "VeryLazy" },
  -- Title case w/ gz<motion>

  { "cosminadrianpopescu/vim-tail", cmd = { "TailStart", "TailStop" } },
  -- Make vim behave like tail -f

  { "guns/xterm-color-table.vim", cmd = "XtermColorTable" },
  -- Preview all 256 xterm colors

  { "moll/vim-bbye", cmd = { "Bdelete", "Bwipeout" } },
  -- Delete buffers without messing up buffer layout

  {
    "AndrewRadev/bufferize.vim",
    cmd = { "Bufferize", "BufferizeSystem", "BufferizeTimer" },
  },
  -- Command contents in buffer

  { "AndrewRadev/linediff.vim", cmd = "Linediff" },
  -- Vimdiff line ranges

  -- No setup }}}

  -- Vim setup {{{
  {
    "lambdalisue/suda.vim",
    -- Give vim sudo powers
    event = "VeryLazy",
    config = function()
      require("pokerus").vimsetup "suda"
    end,
  },

  -- {
  --   "svermeulen/vim-subversive",
  --   -- Substitute from yank
  --   event = "VeryLazy",
  --   config = function()
  --     require("pokerus").vimsetup "subversive"
  --   end,
  -- },

  -- {
  --   "machakann/vim-sandwich",
  --   -- Fancier vim-surround + d
  --   event = "VeryLazy",
  --   config = function()
  --     require("pokerus").vimsetup "sandwich"
  --   end,
  -- },

  {
    "ojroques/vim-oscyank",
    -- Yank across the terminal
    event = "VeryLazy",
    config = function()
      require("pokerus").vimsetup "oscyank"
    end,
  },

  {
    "junegunn/fzf",
    -- The classic fuzzy-finder
    build = function()
      vim.fn["fzf#install"]()
    end,
    config = function()
      require("pokerus").vimsetup "fzf"
    end,
  },
  -- Vim setup }}}

  -- Filetypes {{{
  { "idris-hackers/idris-vim", ft = "idris" },
  { "LnL7/vim-nix", ft = "nix" },
  { "baskerville/vim-sxhkdrc", ft = "sxhkdrc" },
  { "blyoa/vim-promela-syntax", ft = "promela" },
  { "chrisbra/csv.vim", ft = "csv" },
  -- { "rust-lang/rust.vim", ft = "rust" }, -- superseded by rust-tools.nvim
  { "leafgarland/typescript-vim", ft = "typescript" },
  { "keith/swift.vim", ft = "swift" },
  { "dag/vim-fish", ft = "fish" },
  { "cespare/vim-toml", ft = "toml" },
  { "neomutt/neomutt.vim", ft = { "neomuttrc", "mail", "neomuttlog" } },
  { "liuchengxu/graphviz.vim", ft = "dot" },
  { "editorconfig/editorconfig-vim", ft = "dosini" },
  { "mboughaba/i3config.vim", ft = "i3config" },
  { "andy-morris/happy.vim", ft = "happy" },
  { "andy-morris/alex.vim", ft = "alex" },
  { "euclidianAce/BetterLua.vim", ft = "lua" },
  { "mikebentley15/vim-pio", ft = "pio" },
  { "NoahTheDuke/vim-just", ft = "just" },
  { "cfdrake/vim-pbxproj", ft = "pbxproj" },
  { "compnerd/modulemap-vim", ft = "modulemap" },
  -- Filetypes }}}

  -- Filetypes w/ setup {{{
  {
    "fatih/vim-go",
    ft = "go",
    config = function()
      require("pokerus").vimsetup "go"
    end,
  },

  {
    "neovimhaskell/haskell-vim",
    ft = "haskell",
    config = function()
      require("pokerus").vimsetup "haskell"
    end,
  },

  {
    "ziglang/zig.vim",
    ft = "zig",
    config = function()
      require("pokerus").vimsetup "zig"
    end,
  },
  -- Filetypes w/ setup }}}

  -- Filetypes configured elsewhere:
  -- markdown
  -- coq
}
