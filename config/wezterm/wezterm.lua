local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local appearance = require 'appearance'

config.set_environment_variables = {
  PATH = '/opt/homebrew/bin:' .. os.getenv('PATH')
}

-- if appearance.is_dark() then
--   config.color_scheme = 'catppuucin'
-- else
--   config.color_scheme = 'srcery'
-- end

config.color_scheme = 'kanagawa_dragon'
config.font = wezterm.font({ family = 'Berkeley Mono' })
config.font_size = 13
config.default_cursor_style = 'BlinkingBar'
config.window_background_opacity = 0.9
config.macos_window_background_blur = 30
config.window_decorations = 'RESIZE'
config.window_frame = {
  font = wezterm.font({ family = 'Berkeley Mono', weight = 'Bold' }),
  font_size = 11,
}

wezterm.on('update-status', function(window, _)
  local SOLID_LEFT_ARROW = utf8.char(0xe0b2)

  -- Grab the current window's configuration, and from it the
  -- palette (this is the combination of your chosen colour scheme
  -- including any overrides).
  local color_scheme = window:effective_config().resolved_palette
  local bg = color_scheme.background
  local fg = color_scheme.foreground


  window:set_right_status(wezterm.format({
    -- First, we draw the arrow...
    { Background = { Color = 'none' } },
    { Foreground = { Color = bg } },
    { Text = SOLID_LEFT_ARROW },
    -- Then we draw our text
    { Background = { Color = bg } },
    { Foreground = { Color = fg } },
    { Text = ' ' .. wezterm.hostname() .. ' ' },
  }))
end)

return config
