local openbsd = require('openbsd')
local gears = require("gears")
local awful = require("awful")
--local awesompd = require("awesompd/awesompd")

require("awful.autofocus")

local wibox = require("wibox")
local beautiful = require("beautiful")
local naughty = require("naughty")
local hotkeys_popup = require("awful.hotkeys_popup").widget

local textbox = require("wibox.widget.textbox")
local sep = textbox()

beautiful.init("~/.config/awesome/themes/bold_white/theme.lua")

local obsd = require('obsd')
obsd.enable_debug = false

--musicwidget = awesompd:create() -- Create awesompd widget
--musicwidget.font = beautiful.font
--musicwidget.font_color = beautiful.fg_normal
--musicwidget.background = beautiful.bg_normal
--musicwidget.scrolling = true
--musicwidget.output_size = 50
--musicwidget.update_interval = 1 -- Set the update interval in seconds
--musicwidget.jamendo_format = awesompd.FORMAT_MP3
--musicwidget.browser = "browser"
---- musicwidget.show_album_cover = true
---- musicwidget.album_cover_size = 50
--musicwidget.mpd_config = "/etc/mpd.conf"
--
--musicwidget.ldecorator = " "
--musicwidget.rdecorator = " "
--
--musicwidget.servers = {
--   { server = "localhost", port = 6600 }
--}
--
--musicwidget:run()

--local clip = require('clip')
--clip.enable_debug = true

-- Pledge early
--local _, _ = openbsd.pledge('stdio tty rpath wpath cpath proc exec prot_exec unix', 'stdio tty rpath wpath cpath proc unix')
local _, _ = openbsd.pledge('stdio tty rpath wpath cpath proc exec prot_exec unix')

sep.font = beautiful.font
sep.text = " | "

if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

do
   local in_error = false
   awesome.connect_signal("debug::error", function (err)
			     -- Make sure we don't go into an endless error loop
			     if in_error then return end
			     in_error = true
			     naughty.notify({ preset = naughty.config.presets.critical,
					      title = "Oops, an error happened!",
					      text = tostring(err) })
			     in_error = false
   end)
end

local terminal = "xterm"
local rofi = "rofi -show run"

local modkey = "Mod1"

awful.layout.layouts = {
    awful.layout.suit.tile,
    awful.layout.suit.tile.left,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.tile.top,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.max,
    awful.layout.suit.corner.nw,
    awful.layout.suit.floating,
}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- Create a textclock widget
-- TODO convert this to use the calendar api
local mytextclock = wibox.widget.textclock("%a %b %_d (%H)%l:%M%p")

local clock_not = function()
    awful.spawn.easy_async('cal', function(stdout, stderr, reason, exit_code)
	naughty.notify({
		preset = naughty.config.presets.normal,
		title = "Calendar",
		text = tostring(stdout),
	})
    end)
end

mytextclock:buttons(awful.util.table.join(awful.button({ }, 1, clock_not)))

local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 4, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 5, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))

local function set_wallpaper(s)
	if beautiful.wallpaper then
		local wallpaper = beautiful.wallpaper
		if type(wallpaper) == "function" then
			wallpaper = wallpaper(s)
		end
		gears.wallpaper.maximized(wallpaper, s, true)
	end
end

awful.screen.connect_for_each_screen(function(s)
    --set_wallpaper(s)
    awful.tag.add("scratch", {
        layout             = awful.layout.suit.tile,
        screen             = s,
        selected           = true,
    })

    awful.tag.add("browser", {
        layout             = awful.layout.suit.tile,
        screen             = s,
        selected           = false,
    })

    awful.tag.add("irc", {
        layout             = awful.layout.suit.tile,
        screen             = s,
        selected           = false,
    })

    awful.tag.add("mail", {
        layout             = awful.layout.suit.tile,
        screen             = s,
        selected           = false,
    })

    awful.tag.add("5", {screen=s, selected = false, layout = awful.layout.layouts[1] })
    awful.tag.add("6", {screen=s, selected = false, layout = awful.layout.layouts[1] })
    awful.tag.add("7", {screen=s, selected = false, layout = awful.layout.layouts[1] })
    awful.tag.add("8", {screen=s, selected = false, layout = awful.layout.layouts[1] })

    awful.tag.add("console", {
        layout             = awful.layout.suit.tile,
        screen             = s,
        selected           = false,
    })

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))

    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.noempty, taglist_buttons)

    s.mytasklist = awful.widget.tasklist(
      s,
      awful.widget.tasklist.filter.currenttags,
      tasklist_buttons,
      {tasklist_disable_icon = true}
    )

    s.mywibox = awful.wibar({ position = "top", screen = s, height = 18 })

    s.mywibox:setup {
       layout = wibox.layout.align.horizontal,
       { -- Left widgets
	  layout = wibox.layout.fixed.horizontal,
	  obsd.enable_battery(),
	  sep,
	  s.mytaglist,
	  sep,
       },
       s.mytasklist, -- Middle widget
       { -- Right widgets
	  layout = wibox.layout.fixed.horizontal,
	  sep,
      --musicwidget.widget,
	  --sep,
	  mytextclock,
	  sep,
	  wibox.widget.systray(),
      --clip.enable(),
	  obsd.enable_volume(),
	  --obsd.enable_snap(),
	  s.mylayoutbox,
       },
    }
end)

local globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey, "Shift"   }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey,           }, "r", function () awful.spawn(rofi) end,
              {description = "launch rofi", group = "awesome"}),
    awful.key({ modkey, "Shift", "Control" }, "r", awesome.restart,
              {description = "restart awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"}, "r", function () awful.spawn("passmenu") end,
              {description = "run passmenu", group = "launcher"}),
    awful.key({ modkey,           }, "s", hotkeys_popup.show_help,
              {description = "show help", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),
    awful.key({ modkey,           }, "n", function () awful.spawn("/home/qbit/bin/mpcc next") end,
              {description = "next song", group = "mpc"}),
    awful.key({ modkey,           }, "p", function () awful.spawn("/home/qbit/bin/mpcc prev") end,
              {description = "previous song", group = "mpc"}),
    awful.key({ modkey, "Shift"   }, "p", function () awful.spawn("/home/qbit/bin/mpcc toggle") end,
              {description = "toggle music", group = "mpc"}),
    awful.key({ modkey,           }, "e", function () awful.spawn("~/.screenlayout/external.sh") end,
             {description = "use external screen", group = "screen"}),
    awful.key({ modkey,           }, "i", function () awful.spawn("~/.screenlayout/internal.sh") end,
              {description = "use internal screen", group = "screen"}),
    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,  "Control"}, "l",     function () awful.spawn("xlock")                end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"})
)

local clientkeys = awful.util.table.join(
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey,           }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

local clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

root.keys(globalkeys)

awful.rules.rules = {
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen,
		     size_hints_honor = false,
     }
    },

    { rule_any = {
        instance = {
          "DTA",
          "copyq",
          "pinentry",
        },
        class = {
          "MPlayer",
          "mpv",
          "XCalc",
          "pinentry",
          "gcr-prompter",
          "Gcr-prompter",
          "Pinentry-gtk-2",
          "Pinentry-gnome3"},
        name = {
          "Event Tester",  -- xev.
        },
      }, properties = { floating = true }},
    { rule = { class = "Chromium-browser" },
      properties = { screen = 1, tag = "browser" } },
    { rule = { class = "Firefox" },
      properties = { screen = 1, tag = "browser" } },
    { rule = { class = "XConsole" },
      properties = { screen = 1, tag = "console" } },
    { rule = { class = "SshAskpass" },
      properties = { raise = true, focus = awful.client.focus.filter } },
    { rule = { class = "Gcr-prompter" },
      properties = { raise = true, focus = awful.client.focus.filter },
      callback = function (c) awful.placement.centered(c,nil) end,
    }
}
-- }}}

client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    else
      awful.client.setslave(c)
    end
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)

