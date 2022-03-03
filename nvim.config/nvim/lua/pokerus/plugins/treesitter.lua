local M = {}

M.languages = {
  "bash",
  "bibtex",
  "c",
  "cpp",
  "css",
  "dot",
  "fish",
  "go",
  "haskell",
  "html",
  "java",
  "javascript",
  "json",
  "latex",
  "llvm",
  "lua",
  "make",
  "markdown",
  "nix",
  "ocaml",
  "ocaml_interface",
  "python",
  "rst",
  "rust",
  "toml",
  "typescript",
  "vim",
  "yaml",
  "zig",
}

local languages_need_compile = {
  "devicetree",
  "ocamllex",
}

if vim.fn.executable "tree-sitter" ~= 0 then
  for _, v in ipairs(languages_need_compile) do
    table.insert(M.languages, v)
  end
end

function M.plug(use)
  use {
    "nvim-treesitter/nvim-treesitter",
    requires = {
      "nvim-treesitter/nvim-treesitter-refactor",
      -- Use treesitter to refactor identifiers
      "nvim-treesitter/nvim-treesitter-textobjects",
      -- Use treesitter to find motion text objects
      "nvim-treesitter/playground",
      -- Show treesitter state in split pane
      "folke/twilight.nvim",
      -- Dim inactive regions of code
      "danymat/neogen",
      -- Treesitter-powered doc-comment generator
    },
    run = ":TSUpdate",
    config = function()
      require("nvim-treesitter.configs").setup {
        ensure_installed = M.languages,
        playground = {
          enable = true,
        },
        highlight = {
          enable = false, -- I won't have this until conceal via TS is supported
        },
        incremental_selection = {
          enable = true,
          keymaps = {
            init_selection = "<leader>v",
            node_incremental = "<leader>v",
            node_decremental = "<leader>V",
          },
        },
        refactor = {
          smart_rename = {
            enable = true,
            highlight_definitions = { enable = true },
            highlight_current_scope = { enable = true },
            keymaps = {
              smart_rename = "gr",
            },
          },
        },
        textobjects = {
          select = {
            enable = true,
            lookahead = true,
            keymaps = {
              ["a."] = "@function.outer",
              ["i."] = "@function.inner",
              ["cc"] = "@comment.inner",
              ["ac"] = "@class.outer",
              ["ic"] = "@class.inner",
              ["ab"] = "@block.outer",
              ["ib"] = "@block.inner",
            },
          },
          swap = {
            enable = true,
            swap_next = {
              ["gl"] = "@parameter.inner",
            },
            swap_previous = {
              ["gh"] = "@parameter.inner",
            },
          },
        },
      }

      require("pokerus").nmap {
        ["gr"] = "treesitter-refactor",
        ["gl"] = "treesitter-swap-next-param",
        ["gh"] = "treesitter-swap-prev-param",
        ["<leader>v"] = "treesitter-select",
      }

      require("pokerus").omap {
        ["a."] = "treesitter-outer-function",
        ["i."] = "treesitter-outer-function",
        ["cc"] = "treesitter-inner-comment",
        ["ac"] = "treesitter-outer-class",
        ["ic"] = "treesitter-inner-class",
        ["ab"] = "treesitter-outer-block",
        ["ib"] = "treesitter-inner-block",
      }
    end,
  }

  use {
    "lewis6991/spellsitter.nvim",
    -- Show spelling errors
    -- Note that somehow this plugin doesn't actually depend on nvim-treesitter
    config = function()
      require("spellsitter").setup {}
    end,
  }
end
return M