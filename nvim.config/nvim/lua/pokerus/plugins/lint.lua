local linters = {}

local function init_linters()
  local function linter(langs, name, exec)
    if vim.fn.executable(exec or name) == 1 then
      if type(langs) == "string" then
        langs = { langs }
      end
      for _, lang in ipairs(langs) do
        linters[lang] = linters[lang] or {}
        table.insert(linters[lang], name)
      end
    end
  end

  linter({ "c", "cpp" }, "clangtidy", "clang-tidy")
  linter("nix", "statix")
  linter("nix", "nix")
  -- linter("tex", "lacheck") -- not too verbose, but not configurable
  linter("tex", "chktex") -- quite verbose, but configurable. See: https://www.nongnu.org/chktex/ChkTeX.pdf
  linter("lua", "luacheck")
  linter({ "bash", "sh" }, "shellcheck")
  linter("zsh", "zsh")
  linter("haskell", "hlint")
  linter("markdown", "markdownlint")
  linter("swift", "swiftlint")
end

local should_lint = true

local function try_lint()
  if should_lint then
    require("lint").try_lint()
  end
end

local function lint_on()
  should_lint = true
end

local function lint_off()
  should_lint = false
end

local function lint_show()
  local Popup = require("nui.popup")
  local event = require("nui.utils.autocmd").event

  local langs = {}
  local max_width = 0
  for lang in pairs(linters) do
    table.insert(langs, lang)
    max_width = math.max(max_width, #lang)
  end
  table.sort(langs)
  max_width = max_width + 2

  local popup = Popup({
    enter = true,
    focusable = true,
    border = {
      style = "single",
      text = {
        top = " Enabled linters ",
        top_align = "center",
        bottom = " nvim-lint" .. (should_lint and " [ON] " or " [OFF] "),
        bottom_align = "right",
      },
    },
    position = "50%",
    size = {
      width = 40,
      height = math.min(#langs, 20),
    },
  })

  -- mount/open the component
  popup:mount()

  -- unmount component when cursor leaves buffer
  popup:on(event.BufLeave, function()
    popup:unmount()
  end)

  local linenr = 1
  local Line = require("nui.line")

  for _, lang in ipairs(langs) do
    local line = Line()
    line:append(lang .. string.rep(" ", max_width - #lang), "Float")

    for i, lint in ipairs(linters[lang]) do
      if i > 1 then
        line:append(", ", "String")
      end
      line:append(lint, "String")
    end

    line:render(popup.bufnr, -1, linenr)
    linenr = linenr + 1
  end

  vim.api.nvim_buf_set_option(popup.bufnr, "modifiable", false)
  vim.api.nvim_buf_set_option(popup.bufnr, "readonly", true)
  vim.api.nvim_buf_set_keymap(popup.bufnr, "n", "q", "", { callback = function() popup:unmount() end })
end

return {
  "mfussenegger/nvim-lint",
  dependencies = { "MunifTanjim/nui.nvim" },
  config = function()
    init_linters()

    require("lint").linters_by_ft = linters

    vim.api.nvim_create_user_command("LintOn", lint_on, { desc = "Turn on nvim-lint" })
    vim.api.nvim_create_user_command("LintOff", lint_off, { desc = "Turn off nvim-lint" })
    vim.api.nvim_create_user_command("LintShow", lint_show, { desc = "Show nvim-lint status" })

    vim.api.nvim_create_autocmd({ "BufEnter", "BufWritePost" }, {
      callback = try_lint,
    })
  end,
}
