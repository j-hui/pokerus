local M = {}

local languages = {
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

function M.config()
  if vim.fn.executable "tree-sitter" ~= 0 then
    for _, v in ipairs(languages_need_compile) do
      table.insert(languages, v)
    end
  end

  require("nvim-treesitter.configs").setup {
    ensure_installed = languages,
    playground = {
      enable = true,
    },
    query_linter = {
      enable = true,
      use_virtual_text = true,
      lint_events = { "BufWrite", "CursorHold" },
    },
    highlight = {
      enable = true,
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
          ["a$"] = "@function.outer",
          ["i$"] = "@function.inner",
          ["cc"] = "@comment.inner",
          ["a:"] = "@class.outer",
          ["i:"] = "@class.inner",
          ["a;"] = "@block.outer",
          ["i;"] = "@block.inner",
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

  require("spellsitter").setup {}
  require("pokerus").imap({
    s = {
      "<C-g>u<Esc>:lua require'spellsitter'.nav(true)<CR>1z=`]a<C-g>u",
      "correct-spelling",
    },
  }, { prefix = "<C-g>", silent = true, noremap = true })
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
      "lewis6991/spellsitter.nvim",
      -- Show spelling errors in only the right places
    },
    run = ":TSUpdate",
    config = function()
      require("pokerus.plugins.treesitter").config()
    end,
  }
end
return M
