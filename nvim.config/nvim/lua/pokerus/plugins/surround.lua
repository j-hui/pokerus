local ft_opts = {
  markdown = {
    surrounds = {
      -- link
      ["l"] = {
        add = function()
          local link = require("nvim-surround.config").get_input("Link: ")
          return {
            { "[" },
            { "](" .. link .. ")" },
          }
        end,
        find = "%b[]%b()",
        delete = "^(%[)().-(%]%b())()$",
        change = {
          target = "^()()%b[]%((.-)()%)$",
          replacement = function()
            local link = require("nvim-surround.config").get_input("Link: ")
            return {
              { "" },
              { link },
            }
          end,
        },
      },
      -- link text
      ["L"] = {
        add = function()
          local text = require("nvim-surround.config").get_input("Text: ")
          return {
            { "[" .. text .. "](" },
            { ")" },
          }
        end,
        find = "%b[]%b()",
        -- delete = "^(%[)().-(%]%b())()$",
        delete = "^(%b[]%()().-(%))()$",
        change = {
          -- target = "^()()%b[]%((.-)()%)$",
          target = "^%[(.-)()%]%b()()()",
          replacement = function()
            local link = require("nvim-surround.config").get_input("Text: ")
            return {
              { link },
              { "" },
            }
          end,
        },
      }
    },
  },
  tex = {
    surrounds = {
      ["c"] = {
        add = function()
          local cmd = require("nvim-surround.config").get_input("Command: ")
          return { { "\\" .. cmd .. "{" }, { "}" } }
        end,
      },
      ["e"] = {
        add = function()
          local env = require("nvim-surround.config").get_input("Environment: ")
          return { { "\\begin{" .. env .. "}" }, { "\\end{" .. env .. "}" } }
        end,
      },
    },
  },
}

return {
  "kylechui/nvim-surround",
  version = "*",
  event = "VeryLazy",
  dependencies = {
    "nvim-treesitter/nvim-treesitter",
    "nvim-treesitter/nvim-treesitter-textobjects",
  },
  opts = {
    aliases = {
      ["q"] = { '"', "'", "`" },
      ["s"] = { "}", "]", ")", ">", '"', "'", "`" },
    },
    highlight = {
      duration = 0,
    },
    move_cursor = false,
  },
  config = function(_, opts)
    require("nvim-surround").setup(opts)

    for ft, o in pairs(ft_opts) do
      if type(o) == "string" then
        o = ft_opts[o]
      end
      require("pokerus.callback").filetype(ft, function()
        require("nvim-surround").buffer_setup(o)
      end)
    end

    local ft = vim.opt.filetype:get()
    if ft_opts[ft] then
      require("nvim-surround").buffer_setup(ft_opts[ft])
    end
  end,
}
