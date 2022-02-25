local function vimsetup(name)
  return vim.fn["pokerus#plugins#" .. name .. "#setup"]
end

return {
  plug = function(use)
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

    use "farmergreg/vim-lastplace"
    -- Open where last opened

    use "lervag/file-line"
    -- Open file:line

    use "andymass/vim-matchup"
    -- %-navigate user-defined pairs

    use "christoomey/vim-titlecase"
    -- Title case w/ gz<motion>

    use "cosminadrianpopescu/vim-tail"
    -- Make vim behave like tail -f
    -- No setup }}}

    -- Vim setup {{{
    use {
      "lambdalisue/suda.vim",
      -- Give vim sudo powers
      config = vimsetup "suda",
    }

    use {
      "svermeulen/vim-subversive",
      -- Substitute from yank
      config = vimsetup "subversive",
    }

    use {
      "tpope/vim-speeddating",
      -- increment/decrement dates
      config = vimsetup "speeddating",
    }
    use {
      "svermeulen/vim-cutlass",
      -- x and D only delete, no yank/cut but retain cut behavior for d
      config = vimsetup "cutlass",
    }

    use {
      "machakann/vim-sandwich",
      -- Fancier vim-surround + d
      config = vimsetup "sandwich",
    }

    use {
      "ojroques/vim-oscyank",
      -- Yank across the terminal
      config = vimsetup "oscyank",
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
    -- Filetypes }}}

    -- Filetypes w/ setup {{{
    use {
      "fatih/vim-go",
      config = vimsetup "go",
    }

    use {
      "octol/vim-cpp-enhanced-highlight",
      config = vimsetup "cpp_enhanced",
    }

    use {
      "neovimhaskell/haskell-vim",
      config = vimsetup "haskell",
    }

    use {
      "ziglang/zig.vim",
      config = vimsetup "zig",
    }
    -- Filetypes w/ setup }}}

    -- Filetypes configured elsewhere:
    -- markdown
    -- coq
  end,
}
