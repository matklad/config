local wezterm = require 'wezterm'
local balance = require 'balance'

local config = wezterm.config_builder()

config.color_scheme = 'Eighties (base16)'
config.enable_tab_bar = false

config.keys = {
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
