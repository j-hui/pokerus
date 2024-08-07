--[[ Markdown ]]
local markdown_opts = {
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
    },
  },
}

--[[ Tex/LaTeX ]]
local function tex_find_environment()
  local cfg = require("nvim-surround.config")
  if vim.g.loaded_nvim_treesitter then
    local selection = cfg.get_selection {
      node = "generic_environment",
      -- query = {
      --   capture = "@block.outer",
      --   type = "textobjects",
      -- }
      -- NOTE: ^query doesn't seem to work very reliably with LaTeX environments
    }
    if selection then
      return selection
    end
  end
  return cfg.get_selection [[\begin%b{}.-\end%b{}]]
  -- NOTE: ^this does not correctly handle \begin{}-\end{} pairs in all cases
  --        (hence we use treesitter if available)
end

local tex_opts = {
  surrounds = {
    ["c"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local cmd = cfg.get_input("Command: ")
        return { { "\\" .. cmd .. "{" }, { "}" } }
      end,
      find = [=[\[^\{}%[%]]-%b{}]=],
      delete = [[^(\[^\{}]-{)().-(})()$]],
      change = {
        target = [[^\([^\{}]-)()%b{}()()$]],
        replacement = function()
          local cfg = require("nvim-surround.config")
          local cmd = cfg.get_input("Command: ")
          return { { cmd }, { "" } }
        end
      },
    },
    ["C"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local cmd, opts = cfg.get_input("Command: "), cfg.get_input("Options: ")
        return { { "\\" .. cmd .. "[" .. opts .. "]{" }, { "}" } }
      end,
      find = [[\[^\{}]-%b[]%b{}]],
      delete = [[^(\[^\{}]-%b[]{)().-(})()$]],
      change = {
        target = [[^\([^\{}]-)()%[(.*)()%]%b{}$]],
        replacement = function()
          local cfg = require("nvim-surround.config")
          local cmd, opts = cfg.get_input("Command: "), cfg.get_input("Options: ")
          return { { cmd }, { opts } }
        end
      },
    },
    ["e"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local env = cfg.get_input("Environment: ")
        return { { "\\begin{" .. env .. "}" }, { "\\end{" .. env .. "}" } }
      end,
      find = tex_find_environment,
      delete = [[^(\begin%b{})().*(\end%b{})()$]],
      change = {
        target = [[^\begin{(.-)()%}.*\end{(.-)()}$]],
        replacement = function()
          local env = require("nvim-surround.config").get_input("Environment: ")
          return { { env }, { env } }
        end,
      }
    },
    ["E"] = {
      add = function()
        local cfg = require("nvim-surround.config")
        local env, opts = cfg.get_input("Environment: "), cfg.get_input("Options: ")
        return { { "\\begin{" .. env .. "}[" .. opts .. "]" }, { "\\end{" .. env .. "}" } }
      end,
      find = tex_find_environment,
      delete = [[^(\begin%b{}%b[])().*(\end%b{})()$]],
      change = {
        target = [[^\begin%b{}%[(.-)()()()%].*\end%b{}$]],
        replacement = function()
          local cfg = require("nvim-surround.config")
          local env = cfg.get_input("Environment options: ")
          return { { env }, { "" } }
        end,
      }
    },
  },
}

--[[ Rust ]]
local rust_opts = {
  surrounds = {
    -- "generic" (e.g., T -> Box<T>)
    ["g"] = {
      add = function()
        local config = require("nvim-surround.config")
        local result = config.get_input("Enter the generic name: ")
        if result then
          return { { result .. "<" }, { ">" } }
        end
      end,
      find = function()
        local config = require("nvim-surround.config")
        return config.get_selection({ node = "generic_type" })
      end,
      delete = "^(.-<)().-(>)()$",
      change = {
        target = "^(.-<)().-(>)()$",
        replacement = function()
          local config = require("nvim-surround.config")
          local result = config.get_input("Enter the generic name: ")
          if result then
            return { { result .. "<" }, { ">" } }
          end
        end,
      },
    },
  }
}

local ft_opts = {
  markdown = markdown_opts,
  tex = tex_opts,
  plaintex = "tex",
  rust = rust_opts,
}

return {
  "kylechui/nvim-surround",
  version = "v2.1.4",
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
      if type(ft_opts[ft]) == "string" then
        ft = ft_opts[ft]
      end
      require("nvim-surround").buffer_setup(ft_opts[ft])
    end
  end,
}
