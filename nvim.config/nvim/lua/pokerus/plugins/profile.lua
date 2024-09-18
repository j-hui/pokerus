return {} or {
  "stevearc/profile.nvim",
  config = function()
    -- local should_profile = os.getenv("NVIM_PROFILE")
    -- if should_profile then
    --   require("profile").instrument_autocmds()
    --   if should_profile:lower():match("^start") then
    --     require("profile").start("*")
    --   else
    --     require("profile").instrument("*")
    --   end
    -- end
    local function toggle_profile()
      local prof = require("profile")
      if prof.is_recording() then
        prof.stop()
        local default_path = vim.fn.stdpath("log") .. os.date("/profile-%Y%m%d%H%M%S.log")
        vim.ui.input({ prompt = "Save profile to:", completion = "file", default = default_path }, function(filename)
          if filename then
            prof.export(filename)
            vim.notify(string.format("Wrote %s", filename))
          end
        end)
      else
        prof.start("*")
      end
    end
    vim.keymap.set("", "<f1>", toggle_profile)
  end,
}
