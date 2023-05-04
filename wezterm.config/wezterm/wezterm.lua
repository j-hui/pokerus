local keys = require "keys"

return {
  color_scheme = "zenbones_dark",
  disable_default_key_bindings = true,
  keys = keys.keys,
  key_tables = keys.key_tables,
  window_decorations = "RESIZE",
  window_padding = {
    left = '0',
    right = '0',
    top = '3px',
    bottom = '0',
  },
  use_fancy_tab_bar = true,
  scrollback_lines = 69000,
}
