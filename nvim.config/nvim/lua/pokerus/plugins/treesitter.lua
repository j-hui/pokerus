local languages = {
  "bash",
  "bibtex",
  "c",
  "cpp",
  "css",
  "dot",
  "fennel",
  "fish",
  "go",
  "haskell",
  "vimdoc",
  "html",
  "java",
  "javascript",
  "json",
  "latex",
  "llvm",
  "lua",
  "luau",
  "make",
  "markdown",
  "markdown_inline",
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

return {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    -- "nvim-treesitter/nvim-treesitter-refactor",
    -- Use treesitter to refactor identifiers ; tzachar/local-highlight.nvim
    -- promises to be faster for large files
    "nvim-treesitter/nvim-treesitter-textobjects",
    -- Use treesitter to find motion text objects
    "nvim-treesitter/playground",
    -- Show treesitter state in split pane
    "folke/twilight.nvim",
    -- Dim inactive regions of code
    "RRethy/nvim-treesitter-endwise",
    -- Automatically end tokens
  },
  build = ":TSUpdate",
  event = "VeryLazy",
  keys = {
    -- { "gr",        mode = "n", desc = "treesitter-refactor" },
    { "gl",        mode = "n", desc = "treesitter-swap-next-param" },
    { "gh",        mode = "n", desc = "treesitter-swap-prev-param" },
    { "<leader>v", mode = "n", desc = "treesitter-select" },

    { "a.",        mode = "o", desc = "treesitter-outer-function" },
    { "i.",        mode = "o", desc = "treesitter-outer-function" },
    { "cc",        mode = "o", desc = "treesitter-inner-comment" },
    { "ac",        mode = "o", desc = "treesitter-outer-class" },
    { "ic",        mode = "o", desc = "treesitter-inner-class" },
    { "ab",        mode = "o", desc = "treesitter-outer-block" },
    { "ib",        mode = "o", desc = "treesitter-inner-block" },
  },
  config = function()
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
          init_selection = false,
          node_incremental = "<tab>",
          node_decremental = "<S-tab>",
        },
      },
      endwise = {
        enable = true,
      },
      -- refactor = {
      --   highlight_definitions = { enable = true },
      --   -- highlight_current_scope = { enable = true },
      --   smart_rename = {
      --     enable = true,
      --     keymaps = {
      --       smart_rename = "gr",
      --     },
      --   },
      -- },
      textobjects = {
        select = {
          enable = true,
          lookahead = true,
          keymaps = {
            ["af"] = "@function.outer",
            ["if"] = "@function.inner",
            ["ic"] = "@comment.inner",
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
      matchup = {
        enable = true,
      },
    }
  end,
}
