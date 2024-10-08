local wezterm = require 'wezterm'
local balance = require 'balance'

local config = wezterm.config_builder()

  config.tls_clients = {
  {
    name = 'Moby',
    remote_address = '192.168.2.2:8080',
    bootstrap_via_ssh = 'matklad@192.168.2.2',
  },
}

config.keys = {
  {
    key = 'P',
    mods = 'CMD|SHIFT',
    action = wezterm.action.ActivateCommandPalette,
  },
  {
    key = 'LeftArrow',
    mods = 'CMD',
    action = wezterm.action { SendString = "\x1bOH" },
  },
  {
    key = 'RightArrow',
    mods = 'CMD',
    action = wezterm.action { SendString = "\x1bOF" },
  },
  {
    key = 't',
    mods = 'CMD',
    action = wezterm.action.SplitHorizontal {
        domain = 'CurrentPaneDomain'
    },
  },
  {
    key = 'w',
    mods = 'CMD',
    action = wezterm.action.CloseCurrentPane {
      confirm = false,
    },
  },
  {
    key = 'Tab',
    mods = 'CTRL',
    action = wezterm.action.ActivatePaneDirection 'Next',
  },
  {
     key = 'b',
     mods = 'CMD',
     action = wezterm.action.Multiple {
       wezterm.action_callback(balance.balance_panes("x")),
       wezterm.action_callback(balance.balance_panes("y")),
     },
  },
  {
    key = '.',
    mods = 'CMD',
    action = wezterm.action.QuickSelect,
  },
}

return config
