local function format_cmd(args)
  local range = nil
  if args.count ~= -1 then
    local end_line = vim.api.nvim_buf_get_lines(0, args.line2 - 1, args.line2, true)[1]
    range = {
      start = { args.line1, 0 },
      ["end"] = { args.line2, end_line:len() },
    }
  end
  require("conform").format({ async = true, lsp_fallback = true, range = range })
end

vim.api.nvim_create_user_command("Format", format_cmd, { range = true })
vim.api.nvim_create_user_command("Fmt", format_cmd, { range = true })

local function do_format()
  require("conform").format({
    async = true,
    lsp_fallback = true,
  })
end

--- Choose first available formatter.
---
--- From: https://github.com/stevearc/conform.nvim/blob/master/doc/recipes.md#run-the-first-available-formatter-followed-by-more-formatters
---@param bufnr integer
---@param ... string
---@return string
local function first(bufnr, ...)
  local conform = require("conform")
  for i = 1, select("#", ...) do
    local formatter = select(i, ...)
    if conform.get_formatter_info(formatter, bufnr).available then
      return formatter
    end
  end
  return select(1, ...)
end

return {
  "stevearc/conform.nvim",
  event = { "BufWritePre" },
  cmd = { "ConformInfo", "Format", "Fmt" },
  keys = {
    { mode = { "n", "v" }, "<leader>l=", do_format, desc = "format-buffer" },
  },
  opts = {
    -- See: https://github.com/stevearc/conform.nvim?tab=readme-ov-file#formatters
    formatters_by_ft = {
      c = {},
      cpp = {},
      javascript = { "prettierd", "prettier", stop_after_first = true },
      json = { "jq" },
      lua = { "stylua" },
      markdown = function(bufnr)
        return {
          -- first(bufnr, "markdownlint"),
          "markdownlint",
          "mdslw",
          "injected",
        }
      end,
      python = { "isort", "black" },
      rust = { "rustfmt" },
      toml = { "taplo" },
      yaml = { "yq" },
      swift = { "swift_format" }, -- note that "swiftformat" is not an official Swift tool
    },
  },
  init = function()
    -- If you want the formatexpr, here is the place to set it
    vim.o.formatexpr = "v:lua.require'conform'.formatexpr()"
  end,
}
