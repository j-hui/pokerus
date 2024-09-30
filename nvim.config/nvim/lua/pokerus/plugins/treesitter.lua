local M = {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    -- Use treesitter to find motion text objects

    "nvim-treesitter/playground",
    -- Show treesitter state in split pane

    "RRethy/nvim-treesitter-endwise",
    -- Automatically end tokens

    -- "nvim-treesitter/nvim-treesitter-refactor",
    -- Use treesitter to refactor identifiers

  },
  build = ":TSUpdate",
  event = "VeryLazy",
}

M.languages = {
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
  "html",
  "java",
  "javascript",
  "json",
  "just",
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
  "vimdoc",
  "yaml",
  "zig",
}

M.languages_need_compile = {
  "devicetree",
  "ocamllex",
}

M.keys = {
  -- { "gr",        mode = "n", desc = "treesitter-refactor" },
  { "cl",        mode = "n", desc = "treesitter-swap-next-param" },
  { "ch",        mode = "n", desc = "treesitter-swap-prev-param" },
  { "<leader>v", mode = "n", desc = "treesitter-select" },
  { "a.",        mode = "o", desc = "treesitter-outer-function" },
  { "i.",        mode = "o", desc = "treesitter-outer-function" },
  { "cc",        mode = "o", desc = "treesitter-inner-comment" },
  { "ac",        mode = "o", desc = "treesitter-outer-class" },
  { "ic",        mode = "o", desc = "treesitter-inner-class" },
  { "ab",        mode = "o", desc = "treesitter-outer-block" },
  { "ib",        mode = "o", desc = "treesitter-inner-block" },
}

function M.init()
  if vim.fn.executable "tree-sitter" ~= 0 then
    for _, v in ipairs(M.languages_need_compile) do
      table.insert(M.languages, v)
    end
  end
end

function M.config(opts)
  opts.ensure_installed = M.languages
  require("nvim-treesitter.configs").setup(opts)
  vim.opt.foldmethod = "expr"
  vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
end

M.opts = {
  playground = { enable = true },
  highlight = { enable = true },
  endwise = { enable = true },
  query_linter = {
    enable = true,
    use_virtual_text = true,
    lint_events = { "BufWrite", "CursorHold" },
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = false,
      node_incremental = "<tab>",
      node_decremental = "<S-tab>",
    },
  },
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
        ["ax"] = "@call.outer",
        ["ix"] = "@call.inner",
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

return M
