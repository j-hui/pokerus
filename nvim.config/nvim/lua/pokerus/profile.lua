local function ensure_profile()
  local path = vim.fn.stdpath("data") .. "/lazy/profile.nvim"
  if not vim.loop.fs_stat(path) then
    vim.fn.system(({
      "git",
      "clone",
      "--filter=blob:none",
      "https://github.com/stevearc/profile.nvim",
      path
    }))
  end
  vim.opt.rtp:prepend(path)
end

local function enable_profile()
  local should_profile = os.getenv("NVIM_PROFILE")
  if should_profile then
    require("profile").instrument_autocmds()
    if should_profile:lower():match("^start") then
      require("profile").start("*")
    else
      require("profile").instrument("*")
    end
    require("profile").log_instant("pokerus:profile_start")
  end
end

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

return {
  setup = function()
    ensure_profile()
    enable_profile()
    vim.keymap.set("", "<f1>", toggle_profile)
  end,
}
