local wezterm = require 'wezterm'

local config = wezterm.config_builder()

config.color_scheme = 'Alabaster'
config.enable_tab_bar = false

return config
