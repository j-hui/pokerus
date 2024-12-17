local M = {
  "nvim-treesitter/nvim-treesitter",
  dependencies = {
    "nvim-treesitter/nvim-treesitter-textobjects",
    -- Use treesitter to find motion text objects

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

M.actions = {
  next = function()
    local ts = require("nvim-treesitter.ts_utils")
    local src = ts.get_node_at_cursor()
    local dst = src and ts.get_next_node(src, false, false)
    if src and dst and dst:start() - src:start() == 0 then
      ts.goto_node(dst, false, true)
    else
      vim.cmd("normal! w")
    end
  end,
  prev = function()
    local ts = require("nvim-treesitter.ts_utils")
    local src = ts.get_node_at_cursor()
    local dst = src and ts.get_previous_node(src, false, false)
    if src and dst and dst:start() - src:start() == 0 then
      ts.goto_node(dst, false, true)
    else
      vim.cmd("normal! b")
    end
  end,
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

-- NOTE: using the opts parameter from config(opts) seems to cause a stack
-- overflow in treesitter, probably because of some funny business that
-- Lazy is doing. So we use M.opts instead.
function M.config()
  M.opts.ensure_installed = M.languages
  require("nvim-treesitter.configs").setup(M.opts)
  vim.opt.foldmethod = "expr"
  vim.opt.foldexpr = "nvim_treesitter#foldexpr()"
end

M.opts = {
  highlight = {
    enable = true,
    disable = function(lang, buf)
      local max_filesize = 100 * 1024 -- 100 KB
      local ok, stats = pcall(vim.loop.fs_stat, vim.api.nvim_buf_get_name(buf))
      if ok and stats and stats.size > max_filesize then
        return true
      end
    end,
  },
  endwise = { enable = true, },
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
