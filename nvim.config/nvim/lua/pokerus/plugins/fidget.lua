return {
  "j-hui/fidget.nvim",
  dependencies = { "neovim/nvim-lspconfig" },
  dev = true,
  config = function()
    local opts = {
      notification = {
        window = {
          relative = "win",
          -- align = "avoid_cursor",
          -- border_hl = "WarningMsg",
          -- normal_hl = "",
        },
        override_vim_notify = true,
        -- configs = {
        --   default = {
        --     name = "",
        --     ttl = 5,
        --     group_style = "Title",
        --     icon_style = "Special",
        --     annote_style = "Question",
        --     debug_style = "Comment",
        --     warn_style = "WarningMsg",
        --     error_style = "ErrorMsg",
        --   }
        -- },
        configs = {
          default = vim.tbl_extend("force", require('fidget.notification').default_config, {
            icon_on_left = true,
          }),
        }
      },
      progress = {
        -- poll_rate = 0.5,
        -- suppress_done_already = true
        ignore_empty_message = false,
        display = {
          -- progress_icon = function(now) return now % 2 < 1 and "+" or "-" end,
        },
      },
      logger = {
        -- level = vim.log.levels.DEBUG,
      },
    }

    -- Intercept $/progress invocations
    local progress_logger = false
    if progress_logger then
      opts.logger.level = vim.log.levels.DEBUG
      local logger = require("fidget.logger")
      local og = vim.lsp.handlers["$/progress"]
      local function wrapped(x, result, ctx)
        logger.debug("got message from", ctx.client_id, vim.inspect(result))
        local res = og(x, result, ctx)
        logger.debug("nvim handler returned", res)
      end
      vim.lsp.handlers["$/progress"] = wrapped
    end

    local fidget = require("fidget")
    fidget.setup(opts)

    local demo = false
    if demo then
      vim.api.nvim_create_autocmd("LspAttach", {
        callback = function()
          vim.keymap.set("n", "A", function()
            fidget.notify("This is from fidget.notify().")
          end)

          vim.keymap.set("n", "B", function()
            fidget.notify("This is also from fidget.notify().", vim.log.levels.WARN)
          end)

          vim.keymap.set("n", "C", function()
            fidget.notify("fidget.notify() supports annotations...", nil, { annote = "MY NOTE", key = "foobar" })
          end)

          vim.keymap.set("n", "D", function()
            fidget.notify(nil, vim.log.levels.ERROR, { annote = "bottom text", key = "foobar" })
            fidget.notify("... and overwriting notifications.", vim.log.levels.WARN, { annote = "YOUR AD HERE" })
          end)
        end
      })
    end
  end,
}
