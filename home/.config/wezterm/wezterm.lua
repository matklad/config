local wezterm = require 'wezterm'
local balance = require 'balance'

local config = wezterm.config_builder()

wezterm.on('gui-startup', function(cmd)
  local tab, pane, window = wezterm.mux.spawn_window(cmd or {})
  window:gui_window():maximize()
end)

config.color_scheme = 'Tomorrow'
config.enable_tab_bar = false

config.keys = {
  {
    key = 'Escape',
    mods = 'CTRL',
    action = wezterm.action_callback(function(window, pane)
        window:perform_action(wezterm.action.SendKey{ key='c', mods='CTRL' }, pane)
    end),
  },
  {
    key = 'c',
    mods = 'CTRL',
    action = wezterm.action.CopyTo 'ClipboardAndPrimarySelection',
  },
  {
    key = 'v',
    mods = 'CTRL',
    action = wezterm.action.PasteFrom 'Clipboard',
  },
  {
    key = 'e',
    mods = 'SUPER',
    action = wezterm.action.SplitHorizontal {
        domain = 'CurrentPaneDomain'
    },
  },
  {
    key = 'o',
    mods = 'SUPER',
    action = wezterm.action.SplitVertical {
        domain = 'CurrentPaneDomain'
    },
  },
  {
    key = 'n',
    mods = 'SUPER',
    action = wezterm.action.ActivatePaneDirection 'Next',
  },
  {
    key = 'z',
    mods = 'SUPER',
    action = wezterm.action.TogglePaneZoomState,
  },
  {
     key = 'b',
     mods = 'SUPER',
     action = wezterm.action.Multiple {
       wezterm.action_callback(balance.balance_panes("x")),
       wezterm.action_callback(balance.balance_panes("y")),
     },
  },
}

return config
