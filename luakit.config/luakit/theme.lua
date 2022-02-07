--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
theme.font = "15px Monaco, sans-serif"
theme.fg   = "#898989"
theme.bg   = "#1a1a1a"

-- Error colours
theme.error_fg = "#FFF"
theme.error_bg = "#F00"

-- Notification colours
theme.notif_fg = "#d6d6d6"
theme.notif_bg = "#3e3e3e"

-- Menu colours
theme.menu_fg                   = "#000"
theme.menu_bg                   = "#fff"
theme.menu_selected_fg          = "#000"
theme.menu_selected_bg          = "#FF0"
theme.menu_title_bg             = "#fff"
theme.menu_primary_title_fg     = "#f00"
theme.menu_secondary_title_fg   = "#666"

-- Proxy manager
theme.proxy_active_menu_fg      = '#000'
theme.proxy_active_menu_bg      = '#FFF'
theme.proxy_inactive_menu_fg    = '#888'
theme.proxy_inactive_menu_bg    = '#FFF'

-- Statusbar specific
theme.sbar_fg           = "#898989"
theme.sbar_bg           = "#1a1a1a"
theme.loaded_sbar_fg    = "#33AADD"

-- Input bar specific
theme.ibar_fg           = "#d6d6d6"
theme.ibar_bg           = "#3e3e3e"

-- Tab label
theme.tab_fg            = "#888"
theme.tab_bg            = "#222"
theme.selected_fg       = "#fff"
theme.selected_bg       = "#000"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#000"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#0F0"
theme.notrust_fg        = "#F00"

-- Hint styling
theme.hint_font = theme.font
theme.hint_bg = theme.bg
theme.hint_fg = theme.fg

theme.ok = {}
theme.ok.fg = theme.fg
theme.ok.bg = theme.bg

return theme
-- vim: et:sw=4:ts=8:sts=4:tw=80
