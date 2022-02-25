return {
  plug = function(use)
    use {
      "kana/vim-textobj-user",
      -- User-defined motions beginning with {a,i}
      requires = {
        "thinca/vim-textobj-between",
        -- {a,i} f<char> motions for region between <char>
        "Julian/vim-textobj-variable-segment",
        -- {a,i} v motions for portions of variables

        "kana/vim-textobj-indent",
        -- {a,i} i motions for indented blocks of text

        "rhysd/vim-textobj-continuous-line",
        -- motions for navigating line continuations

        "kana/vim-textobj-syntax",
        -- {a,i} y motions for syntax items

        "kana/vim-textobj-datetime",
        -- {a,i} da, dd, df, dt, dz motions for date/time

        "rbonvall/vim-textobj-latex",
        -- {a,i} $, q/Q, and e motions for latex math, quotes, and environments

        "paulhybryant/vim-textobj-path",
        -- {a,i} {p,P} for path names

        "adriaanzon/vim-textobj-matchit",
        -- {a,i} m for matchit pairs
      },
      config = function()
        vim.g.textobj_continuous_line_no_default_key_mappings = 1
        vim.g.textobj_continuous_line_no_default_mappings = 1
        vim.g.textobj_matchit_no_default_key_mappings = 1

        require("pokerus").omap {
          ["a\\"] = "<Plug>(textobj-continuous-vim-a)",
          ["i\\"] = "<Plug>(textobj-continuous-vim-i)",
          ["a%"] = "<Plug>(textobj-matchit-a)",
          ["i%"] = "<Plug>(textobj-matchit-i)",
        }
        require("pokerus").xmap {
          ["a\\"] = "<Plug>(textobj-continuous-vim-a)",
          ["i\\"] = "<Plug>(textobj-continuous-vim-i)",
          ["a%"] = "<Plug>(textobj-matchit-a)",
          ["i%"] = "<Plug>(textobj-matchit-i)",
        }
      end,
    }
  end,
}
