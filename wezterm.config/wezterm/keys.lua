local wezterm = require 'wezterm'
local act = wezterm.action

local M = {}

-- WIP: missing features:
-- - persistent sessions
-- - splits
-- - selection (w/ hints)
-- - emoji insertion?
--
-- which keys should I use? I have a, e, s, y, i, and m left, plus symbols

M.keys = {}

local mod_keybinds = {
  ["r"] = act.ReloadConfiguration,
  ["q"] = act.QuitApplication,
  ["w"] = act.CloseCurrentTab { confirm = true },
  ["o"] = act.SpawnWindow,
  ["t"] = act.SpawnTab "CurrentPaneDomain",
  ["-"] = act.DecreaseFontSize,
  ["="] = act.IncreaseFontSize,
  ["0"] = act.ResetFontSize,

  ["z"] = act.ShowDebugOverlay,
  ["Comma"] = act.MoveTabRelative(-1),
  ["Period"] = act.MoveTabRelative(1),
  ["["] = act.ActivateTabRelative(-1),
  ["]"] = act.ActivateTabRelative(1),
  ["b"] = act.ActivateTabRelative(-1),
  ["f"] = act.ActivateTabRelative(1),

  ["x"] = act.ActivateCopyMode,
  ["c"] = act.CopyTo "Clipboard",
  ["v"] = act.PasteFrom "Clipboard",

  ["Slash"] = act.Search "CurrentSelectionOrEmptyString",
  -- ["Semicolon"] = act.Search "CurrentSelectionOrEmptyString",
  -- ["Quote"] = act.Search "CurrentSelectionOrEmptyString",
  ["5"] = act.Search "CurrentSelectionOrEmptyString",

  ["u"] = act.ScrollByPage(-1),
  ["d"] = act.ScrollByPage(1),
  ["p"] = act.ScrollByLine(-1),
  ["n"] = act.ScrollByLine(1),

  ["g"] = act.PaneSelect { alphabet = '', mode = 'Activate' },

  ["h"] = act.ActivatePaneDirection "Left",
  ["j"] = act.ActivatePaneDirection "Down",
  ["k"] = act.ActivatePaneDirection "Up",
  ["l"] = act.ActivatePaneDirection "Right",
  ["LeftArrow"] = act.ActivatePaneDirection "Left",
  ["DownArrow"] = act.ActivatePaneDirection "Down",
  ["UpArrow"] = act.ActivatePaneDirection "Up",
  ["RightArrow"] = act.ActivatePaneDirection "Right",
}

local additional_keybinds = {
  { key = "Tab", mods = "CTRL", action = act.ActivateTabRelative(1) },
  { key = "Tab", mods = "SHIFT|CTRL", action = act.ActivateTabRelative(-1) },
  { key = "Insert", mods = "SHIFT", action = act.PasteFrom "PrimarySelection" },
  { key = "Insert", mods = "CTRL", action = act.CopyTo "PrimarySelection" },
  { key = "Copy", mods = "NONE", action = act.CopyTo "Clipboard" },
  { key = "Paste", mods = "NONE", action = act.PasteFrom "Clipboard" },
  { key = 'PageUp', mods = 'SHIFT', action = act.ScrollByPage(-1) },
  { key = 'PageDown', mods = 'SHIFT', action = act.ScrollByPage(1) },
  { key = 'phys:Space', mods = 'SHIFT|CTRL', action = act.QuickSelect },
}

for k, a in pairs(mod_keybinds) do
  table.insert(M.keys, {key = k, mods = "SHIFT|CTRL", action = a})
  -- TODO: only add this on macOS
  table.insert(M.keys, {key = k, mods = "SUPER", action = a})
end

for _, b in ipairs(additional_keybinds) do
  table.insert(M.keys, b)
end

-- M.keys = {
--   -- Window management
--   { key = 'Enter', mods = 'ALT', action = act.ToggleFullScreen },
--   { key = 'H', mods = 'CTRL', action = act.HideApplication },
--   { key = 'H', mods = 'SHIFT|CTRL', action = act.HideApplication },
--   { key = 'M', mods = 'CTRL', action = act.Hide },
--   { key = 'M', mods = 'SHIFT|CTRL', action = act.Hide },
--   { key = 'm', mods = 'SHIFT|CTRL', action = act.Hide },
--   { key = 'm', mods = 'SUPER', action = act.Hide },
--   { key = 'h', mods = 'SHIFT|CTRL', action = act.HideApplication },
--   { key = 'h', mods = 'SUPER', action = act.HideApplication },
--
--   -- Splits
--   { key = '"', mods = 'ALT|CTRL', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
--   { key = '"', mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
--   { key = "'", mods = 'SHIFT|ALT|CTRL', action = act.SplitVertical { domain = 'CurrentPaneDomain' } },
--   { key = '%', mods = 'ALT|CTRL', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
--   { key = '%', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
--   { key = '5', mods = 'SHIFT|ALT|CTRL', action = act.SplitHorizontal { domain = 'CurrentPaneDomain' } },
--
--   -- Insert unicode
--   { key = 'U', mods = 'CTRL', action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
--   { key = 'U', mods = 'SHIFT|CTRL',
--     action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
--   { key = 'u', mods = 'SHIFT|CTRL',
--     action = act.CharSelect { copy_on_select = true, copy_to = 'ClipboardAndPrimarySelection' } },
--
--   { key = 'P', mods = 'CTRL', action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
--   { key = 'P', mods = 'SHIFT|CTRL', action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
--   { key = 'p', mods = 'SHIFT|CTRL', action = act.PaneSelect { alphabet = '', mode = 'Activate' } },
--
--   { key = 'Z', mods = 'CTRL', action = act.TogglePaneZoomState },
--   { key = 'Z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
--   { key = 'z', mods = 'SHIFT|CTRL', action = act.TogglePaneZoomState },
--
-- }
M.key_tables = {
  copy_mode = {
    { key = 'Tab', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
    { key = 'Tab', mods = 'SHIFT', action = act.CopyMode 'MoveBackwardWord' },
    { key = 'Enter', mods = 'NONE', action = act.CopyMode 'MoveToStartOfNextLine' },
    { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
    { key = '[', mods = 'CTRL', action = act.CopyMode 'Close' },
    { key = 'Space', mods = 'NONE', action = act.CopyMode { SetSelectionMode = 'Cell' } },
    { key = '$', mods = 'NONE', action = act.CopyMode 'MoveToEndOfLineContent' },
    { key = '$', mods = 'SHIFT', action = act.CopyMode 'MoveToEndOfLineContent' },
    { key = 'e', mods = 'CTRL', action = act.CopyMode 'MoveToEndOfLineContent' },
    { key = ',', mods = 'NONE', action = act.CopyMode 'JumpReverse' },
    { key = '0', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLine' },
    { key = ';', mods = 'NONE', action = act.CopyMode 'JumpAgain' },
    { key = 'F', mods = 'NONE', action = act.CopyMode { JumpBackward = { prev_char = false } } },
    { key = 'F', mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = false } } },
    { key = 'G', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackBottom' },
    { key = 'G', mods = 'SHIFT', action = act.CopyMode 'MoveToScrollbackBottom' },
    { key = 'H', mods = 'NONE', action = act.CopyMode 'MoveToViewportTop' },
    { key = 'H', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportTop' },
    { key = 'L', mods = 'NONE', action = act.CopyMode 'MoveToViewportBottom' },
    { key = 'L', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportBottom' },
    { key = 'M', mods = 'NONE', action = act.CopyMode 'MoveToViewportMiddle' },
    { key = 'M', mods = 'SHIFT', action = act.CopyMode 'MoveToViewportMiddle' },
    { key = 'O', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
    { key = 'O', mods = 'SHIFT', action = act.CopyMode 'MoveToSelectionOtherEndHoriz' },
    { key = 'T', mods = 'NONE', action = act.CopyMode { JumpBackward = { prev_char = true } } },
    { key = 'T', mods = 'SHIFT', action = act.CopyMode { JumpBackward = { prev_char = true } } },
    { key = 'V', mods = 'NONE', action = act.CopyMode { SetSelectionMode = 'Line' } },
    { key = 'V', mods = 'SHIFT', action = act.CopyMode { SetSelectionMode = 'Line' } },
    { key = '^', mods = 'NONE', action = act.CopyMode 'MoveToStartOfLineContent' },
    { key = '^', mods = 'SHIFT', action = act.CopyMode 'MoveToStartOfLineContent' },
    { key = 'a', mods = 'CTRL', action = act.CopyMode 'MoveToStartOfLineContent' },
    { key = 'b', mods = 'NONE', action = act.CopyMode 'MoveBackwardWord' },
    { key = 'b', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
    { key = 'u', mods = 'CTRL', action = act.CopyMode 'PageUp' },
    { key = 'c', mods = 'CTRL', action = act.CopyMode 'Close' },
    { key = 'f', mods = 'NONE', action = act.CopyMode { JumpForward = { prev_char = false } } },
    { key = 'f', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
    { key = 'd', mods = 'CTRL', action = act.CopyMode 'PageDown' },
    { key = 'g', mods = 'NONE', action = act.CopyMode 'MoveToScrollbackTop' },
    { key = 'g', mods = 'CTRL', action = act.CopyMode 'Close' },
    { key = 'h', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
    { key = 'j', mods = 'NONE', action = act.CopyMode 'MoveDown' },
    { key = 'k', mods = 'NONE', action = act.CopyMode 'MoveUp' },
    { key = 'l', mods = 'NONE', action = act.CopyMode 'MoveRight' },
    { key = 'n', mods = 'CTRL', action = act.CopyMode 'MoveDown' },
    { key = 'p', mods = 'CTRL', action = act.CopyMode 'MoveUp' },
    { key = 'f', mods = 'CTRL', action = act.CopyMode 'MoveRight' },
    { key = 'b', mods = 'CTRL', action = act.CopyMode 'MoveLeft' },
    { key = 'm', mods = 'ALT', action = act.CopyMode 'MoveToStartOfLineContent' },
    { key = 'o', mods = 'NONE', action = act.CopyMode 'MoveToSelectionOtherEnd' },
    { key = 'q', mods = 'NONE', action = act.CopyMode 'Close' },
    { key = 't', mods = 'NONE', action = act.CopyMode { JumpForward = { prev_char = true } } },
    { key = 'v', mods = 'NONE', action = act.CopyMode { SetSelectionMode = 'Cell' } },
    { key = 'v', mods = 'CTRL', action = act.CopyMode { SetSelectionMode = 'Block' } },
    { key = 'w', mods = 'NONE', action = act.CopyMode 'MoveForwardWord' },
    { key = 'y', mods = 'NONE',
      action = act.Multiple { { CopyTo = 'ClipboardAndPrimarySelection' }, { CopyMode = 'Close' } } },
    { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PageUp' },
    { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'PageDown' },
    { key = 'LeftArrow', mods = 'NONE', action = act.CopyMode 'MoveLeft' },
    { key = 'LeftArrow', mods = 'ALT', action = act.CopyMode 'MoveBackwardWord' },
    { key = 'RightArrow', mods = 'NONE', action = act.CopyMode 'MoveRight' },
    { key = 'RightArrow', mods = 'ALT', action = act.CopyMode 'MoveForwardWord' },
    { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'MoveUp' },
    { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'MoveDown' },
  },

  search_mode = {
    { key = 'Enter', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
    { key = 'Escape', mods = 'NONE', action = act.CopyMode 'Close' },
    { key = '[', mods = 'CTRL', action = act.CopyMode 'Close' },
    { key = 'n', mods = 'CTRL', action = act.CopyMode 'NextMatch' },
    { key = 'p', mods = 'CTRL', action = act.CopyMode 'PriorMatch' },
    { key = 'r', mods = 'CTRL', action = act.CopyMode 'CycleMatchType' },
    { key = 'u', mods = 'CTRL', action = act.CopyMode 'ClearPattern' },
    { key = 'PageUp', mods = 'NONE', action = act.CopyMode 'PriorMatchPage' },
    { key = 'PageDown', mods = 'NONE', action = act.CopyMode 'NextMatchPage' },
    { key = 'UpArrow', mods = 'NONE', action = act.CopyMode 'PriorMatch' },
    { key = 'DownArrow', mods = 'NONE', action = act.CopyMode 'NextMatch' },
  },

}

return M
