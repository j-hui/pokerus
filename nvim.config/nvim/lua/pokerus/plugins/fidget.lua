return {
  "j-hui/fidget.nvim",
  dependencies = { "neovim/nvim-lspconfig" },
  dev = true,
  config = function()
    local opts = {
      notification = {
        window = {
          relative = "editor",
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
            -- icon_on_left = false,
            -- update_hook = false,
          }),
        }
      },
      progress = {
        -- poll_rate = 0.5,
        -- suppress_done_already = true
        ignore_empty_message = false,
        display = {
          -- progress_icon = 2 , -- bad pattern
          -- progress_icon = { 2 }, -- bad pattern
          -- progress_icon = {
          -- pattern = 3 , -- { "⣷", "⣯", "⣟", "⡿", "⢿", "⣻", "⣽", "⣾" },
          -- },
          -- progress_icon = function(now) return now % 2 < 1 and "+" or "-" end,
          -- overrides = {
            -- rust_analyzer = {
            --   name = "Rust Analyzer",
            --   icon = require("fidget.progress.display").for_icon(
            --     require("fidget.spinner").animate({
            --       pattern = require("fidget.spinner.patterns").arrow,
            --       period = 2.5,
            --     }),
            --     "🦀"
            --   ),
            --   update_hook = function(item)
            --     require("fidget.notification").set_content_key(item)
            --     if item.hidden == nil and string.match(item.annote, "clippy") then
            --       -- Hide clippy-related notifications
            --       item.hidden = true
            --     end
            --   end,
            -- },
          -- }
        },
        -- ignore = { "null-ls" },
        lsp = {
          -- log_handler = true,
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
