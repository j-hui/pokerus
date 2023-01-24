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
  "help",
  "html",
  "java",
  "javascript",
  "json",
  "latex",
  "llvm",
  "lua",
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
    "nvim-treesitter/nvim-treesitter-refactor",
    -- Use treesitter to refactor identifiers
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
  keys = {
    { "gr", mode = "n", desc = "treesitter-refactor" },
    { "gl", mode = "n", desc = "treesitter-swap-next-param" },
    { "gh", mode = "n", desc = "treesitter-swap-prev-param" },
    { "<leader>v", mode = "n", desc = "treesitter-select" },

    { "a.", mode = "o", desc = "treesitter-outer-function" },
    { "i.", mode = "o", desc = "treesitter-outer-function" },
    { "cc", mode = "o", desc = "treesitter-inner-comment" },
    { "ac", mode = "o", desc = "treesitter-outer-class" },
    { "ic", mode = "o", desc = "treesitter-inner-class" },
    { "ab", mode = "o", desc = "treesitter-outer-block" },
    { "ib", mode = "o", desc = "treesitter-inner-block" },
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
          init_selection = "<leader>v",
          node_incremental = "<leader>v",
          node_decremental = "<leader>V",
        },
      },
      endwise = {
        enable = true,
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
  end,
}
