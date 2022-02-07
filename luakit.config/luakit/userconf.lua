local adblock = require("adblock")
local adblock_chrome = require("adblock_chrome")

local theme = require("theme")

local settings = require("settings")
local engines = settings.window.search_engines
engines.default = engines.duckduckgo
settings.application.prefer_dark_mode = true
settings.session.always_save = true
-- settings.tablist.visibility = "always"
settings.webview.javascript_can_access_clipboard = true
settings.webview.hardware_acceleration_policy = "always"
settings.window.home_page = "https://duckduckgo.com"

local newtab = require("newtab_chrome")
newtab.new_tab_src = ([==[
    <html>
        <head><title>New Tab</title></head>
        <body bgcolor="{bgcolor}"></body>
    </html>
]==]):gsub("{bgcolor}", theme.bg)

local editor = require("editor")
editor.editor_cmd = "alacritty --class floatterm -e nvim {file} +{line}"

local select = require("select")
select.label_maker = function()
  local chars = charset("sdfqwrzxcvtgb")
  return trim(sort(reverse(chars)))
end

local follow = require("follow")
follow.ignore_delay = 269
follow.pattern_maker = function(s)
  if s:sub(-1) == "=" then
    return follow.pattern_styles.match_label(s:sub(1, -2))
  else
    return follow.pattern_styles.match_label_re_text(s)
  end
end
-- NOTE: to escape -, type %-

local modes = require("modes")
modes.remap_binds("normal", {
  -- new, old, keep (default: false)
  { ",", ";", true },
  { ";", ":", true },
  { "c", "O", false },
  { "O", "t", false },
})

modes.add_binds("normal", {
  {
    "<Control-c>",
    "Copy selected text.",
    function()
      luakit.selection.clipboard = luakit.selection.primary
    end,
  },
  {
    "y",
    "Yank current URI to clipboard.",
    function(w)
      local uri = string.gsub(w.view.uri or "", " ", "%%20")
      luakit.selection.clipboard = uri
      w:notify("Yanked uri (to clipboard): " .. uri)
    end,
  },
})

modes.remap_binds("insert", {
  { "<Control-o>", "<Control-e>", false },
})

modes.remap_binds("completion", {
  { "<Control-n>", "<Down>", true },
  { "<Control-p>", "<Up>", true },
})

local function mpv(uri)
  luakit.spawn(
    string.format(
      "mpv --autofit=100%%x100%% --force-window=immediate --keep-open=yes --ytdl-format=bestvideo[height<=?720]+bestaudio/best %s",
      uri
    )
  )
end

modes.add_binds("ex-follow", {
  -- Yank element uri to open in an external application
  {
    "d",
    [[Hint all links (as defined by the `follow.selectors.uri`
        selector) and set the primary selection to the matched elements URI,
        so that an external app can open it]],
    function(w)
      w:set_mode("follow", {
        prompt = "video",
        selector = "uri",
        evaluator = "uri",
        func = function(uri)
          assert(type(uri) == "string")
          uri = string.gsub(uri, " ", "%%20")
          luakit.selection.primary = uri
          if string.match(uri, "youtube") then
            mpv(uri)
            w:notify("trying to play file on mpv " .. uri)
          elseif string.match(uri, "vimeo") then
            mpv(uri)
            w:notify("trying to play file on mpv " .. uri)
          elseif string.match(uri, "vine") then
            mpv(uri)
            w:notify("trying to play file on mpv " .. uri)
            -- elseif string.match(uri, "pdf" or "PDF") then
            --   luakit.spawn(string.format("~/.config/scripts/pdfFromURL %s", uri))
            -- w:notify("trying to read file via zathura ")
            -- elseif string.match(uri, "jpg") then
            --   luakit.spawn(string.format("feh -x %s", uri))
            --   w:notify("file contains jpg ")
            -- else
            --   luakit.spawn(string.format("feh -x %s.jpg", uri))
            --   w:notify("no jpg extension | unrecognized")
          end
        end,
      })
    end,
  },
})

local cmdhist = require("cmdhist")
cmdhist.history_prev = "<Control-p>"
cmdhist.history_next = "<Control-n>"
