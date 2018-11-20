local theme = {}

theme.font          = "Terminus 9"

--theme.bg_normal     = "#222222"
theme.bg_normal     = "#CCCCCC"
theme.bg_focus      = theme.bg_normal
theme.bg_urgent     = "#ff0000"
theme.bg_minimize   = "#444444"
theme.bg_systray    = theme.bg_normal

theme.fg_normal     = "#000000"
theme.fg_focus      = "#ffffff"
theme.fg_urgent     = "#ffffff"
theme.fg_minimize   = "#ffffff"

theme.battery_high     = "#66d93c"
theme.battery_medium   = "#edf940"
theme.battery_low      = "#db2a20"
theme.battery_bg       = theme.bg_normal
theme.battery_border   = "#000000"

theme.snap_old      = "#cccccc"
theme.snap_new      = "#66d93c"
theme.snap_border   = "#000000"

theme.useless_gap   = 2
theme.border_width  = 1
theme.border_normal = "#000000"
theme.border_focus  = "#535d6c"
theme.border_marked = "#91231c"

theme.clip_icon = "~/.config/awesome/themes/bold_white/clip_icon.png"

-- Define the image to load
theme.titlebar_close_button_normal = "/usr/local/share/awesome/themes/default/titlebar/close_normal.png"
theme.titlebar_close_button_focus  = "/usr/local/share/awesome/themes/default/titlebar/close_focus.png"

theme.titlebar_minimize_button_normal = "/usr/local/share/awesome/themes/default/titlebar/minimize_normal.png"
theme.titlebar_minimize_button_focus  = "/usr/local/share/awesome/themes/default/titlebar/minimize_focus.png"

theme.titlebar_ontop_button_normal_inactive = "/usr/local/share/awesome/themes/default/titlebar/ontop_normal_inactive.png"
theme.titlebar_ontop_button_focus_inactive  = "/usr/local/share/awesome/themes/default/titlebar/ontop_focus_inactive.png"
theme.titlebar_ontop_button_normal_active = "/usr/local/share/awesome/themes/default/titlebar/ontop_normal_active.png"
theme.titlebar_ontop_button_focus_active  = "/usr/local/share/awesome/themes/default/titlebar/ontop_focus_active.png"

theme.titlebar_sticky_button_normal_inactive = "/usr/local/share/awesome/themes/default/titlebar/sticky_normal_inactive.png"
theme.titlebar_sticky_button_focus_inactive  = "/usr/local/share/awesome/themes/default/titlebar/sticky_focus_inactive.png"
theme.titlebar_sticky_button_normal_active = "/usr/local/share/awesome/themes/default/titlebar/sticky_normal_active.png"
theme.titlebar_sticky_button_focus_active  = "/usr/local/share/awesome/themes/default/titlebar/sticky_focus_active.png"

theme.titlebar_floating_button_normal_inactive = "/usr/local/share/awesome/themes/default/titlebar/floating_normal_inactive.png"
theme.titlebar_floating_button_focus_inactive  = "/usr/local/share/awesome/themes/default/titlebar/floating_focus_inactive.png"
theme.titlebar_floating_button_normal_active = "/usr/local/share/awesome/themes/default/titlebar/floating_normal_active.png"
theme.titlebar_floating_button_focus_active  = "/usr/local/share/awesome/themes/default/titlebar/floating_focus_active.png"

theme.titlebar_maximized_button_normal_inactive = "/usr/local/share/awesome/themes/default/titlebar/maximized_normal_inactive.png"
theme.titlebar_maximized_button_focus_inactive  = "/usr/local/share/awesome/themes/default/titlebar/maximized_focus_inactive.png"
theme.titlebar_maximized_button_normal_active = "/usr/local/share/awesome/themes/default/titlebar/maximized_normal_active.png"
theme.titlebar_maximized_button_focus_active  = "/usr/local/share/awesome/themes/default/titlebar/maximized_focus_active.png"

theme.wallpaper = "~/.background.png"

-- You can use your own layout icons like this:
theme.layout_fairh = "/usr/local/share/awesome/themes/default/layouts/fairh.png"
theme.layout_fairv = "/usr/local/share/awesome/themes/default/layouts/fairv.png"
theme.layout_floating  = "/usr/local/share/awesome/themes/default/layouts/floating.png"
theme.layout_magnifier = "/usr/local/share/awesome/themes/default/layouts/magnifier.png"
theme.layout_max = "/usr/local/share/awesome/themes/default/layouts/max.png"
theme.layout_fullscreen = "/usr/local/share/awesome/themes/default/layouts/fullscreen.png"
theme.layout_tilebottom = "/usr/local/share/awesome/themes/default/layouts/tilebottom.png"
theme.layout_tileleft   = "/usr/local/share/awesome/themes/default/layouts/tileleft.png"
theme.layout_tile = "/usr/local/share/awesome/themes/default/layouts/tile.png"
theme.layout_tiletop = "/usr/local/share/awesome/themes/default/layouts/tiletop.png"
theme.layout_spiral  = "/usr/local/share/awesome/themes/default/layouts/spiral.png"
theme.layout_dwindle = "/usr/local/share/awesome/themes/default/layouts/dwindle.png"
theme.layout_cornernw = "/usr/local/share/awesome/themes/default/layouts/cornernw.png"
theme.layout_cornerne = "/usr/local/share/awesome/themes/default/layouts/cornerne.png"
theme.layout_cornersw = "/usr/local/share/awesome/themes/default/layouts/cornersw.png"
theme.layout_cornerse = "/usr/local/share/awesome/themes/default/layouts/cornerse.png"

theme.awesome_icon = "/usr/local/share/awesome/icons/awesome16.png"

-- Define the icon theme for application icons. If not set then the icons
-- from /usr/share/icons and /usr/share/icons/hicolor will be used.
theme.icon_theme = nil

return theme

-- vim: filetype=lua:expandtab:shiftwidth=4:tabstop=8:softtabstop=4:textwidth=80
