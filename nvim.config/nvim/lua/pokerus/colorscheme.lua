local M = {}

M.colorschemes = {
  {
    "nyoom-engineering/oxocarbon.nvim",
    schemename = "oxocarbon",
  },
  {
    "folke/tokyonight.nvim",
    schemename = "tokyonight",
    -- main = true,
    opts = {
      style = "night",
      styles = {
        keywords = { italic = false }
      },
      sidebars = { "qf", "vista_kind", "terminal", "packer" }
    },
  },
  {
    "Shatur/neovim-ayu",
    schemename = "ayu",
    main = true,
    opts = {
      mirage = true,
    },
  },
  {
    "catppuccin/nvim",
    name = "catppuccin",
    schemename = "catppuccin",
    -- main = true,
    opts = {
      flavour = "mocha",
    },
  },
  {
    "neanias/everforest-nvim",
    schemename = "everforest",
    -- main = true,
    opts = {
      background = "hard",
    },
  },
  {
    "Everblush/nvim",
    -- main = true,
    schemename = "everblush",
    name = "everblush",
    opts = {
      override = {
        -- Comments are too dark
        Comment = { fg = "#606769" },
        ["@comment"] = { fg = "#606769" },

        -- Floating point numbers are aggressively dimmed for some reason
        ["@float"] = { fg = "@number" },
      },
    },
  },
}

for _, colo in ipairs(M.colorschemes) do
  if not colo.schemename then
    vim.notify("Error: did not specify colorscheme name for " .. colo[1], vim.log.levels.ERROR)
  else
    colo.lazy, colo.priority = true, 1000

    if colo.main then
      colo.lazy = false
      M.main = colo.schemename
    end

    colo.config = function(_, opts)
      if colo.opts then
        require(colo.modname or colo.schemename).setup(opts)
      end
      vim.cmd.colorscheme(colo.schemename)
    end
  end
end

if not M.main then
  vim.notify("Warning: no main theme specified. Defaulting to habamax.", vim.log.levels.WARN)
  M.main = "habamax"
end

local function setup_colo_overrides()
  local function colo_overrides()
    local signs = {
      { name = "DiagnosticSignError", text = "✖" },
      { name = "DiagnosticSignWarn", text = "‼" },
      { name = "DiagnosticSignHint", text = "ℹ" },
      { name = "DiagnosticSignInfo", text = "»" },
    }

    for _, sign in ipairs(signs) do
      vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
    end

    vim.api.nvim_set_hl(0, "LspReferenceRead", { underline = true })
    vim.api.nvim_set_hl(0, "LspReferenceWrite", { underline = true })
    vim.api.nvim_set_hl(0, "LspReferenceText", { underline = true })
  end

  colo_overrides()
  require("pokerus.callback").colorscheme(colo_overrides)
end

function M.setup()
  setup_colo_overrides()
end

return M
