local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
--local wibox = require("wibox")
local clip = {}

clip.enable_debug = false
clip.combine = true
clip.store = {}
clip.max = 5

clip.menu_items = awful.menu({ items = {
  {"status", function()
    local state = "Not running"
    if clip.grab_timer.started then
      state = "Running"
    end

    naughty.notify({ preset = naughty.config.presets.normal,
                     title = "Grab Status",
                     text = state})
  end},
  {"clear", function()
    clip.store = {}
    clip.menu_items:hide()
    clip.delete_menu_items()
    -- excessive!
    awful.spawn.easy_async('xsel -cp', function() end)
    awful.spawn.easy_async('xsel -cb', function() end)
    awful.spawn.easy_async('xsel -cs', function() end)
    awful.spawn.easy_async('xsel -dp', function() end)
    awful.spawn.easy_async('xsel -db', function() end)
    awful.spawn.easy_async('xsel -ds', function() end)
  end, beautiful.clip_icon}
}})

function clip.delete_menu_items()
  clip.menu_items:update()
  for i = 1, #clip.menu_items.items do
    if clip.menu_items.items[i] and
      clip.menu_items.items[i].label.text ~= "clear" and
      clip.menu_items.items[i].label.text ~= "status" then
      clip.menu_items:delete(clip.menu_items.items[i])
    end
  end
  clip.menu_items:update()
end

function clip.sync_items()
  clip.delete_menu_items()

  for i = 1, #clip.store do
    print('adding: ', clip.store[i])
    clip.menu_items:add({clip.store[i], function()
      awful.spawn.easy_async('xsel -i', function() end)
    end}, 1)
  end
  clip.menu_items:update()
end

clip.grab_timer = gears.timer({
  timeout = 1,
  autostart = true,
  call_now  = true,
  callback = function()
    clip.add()
  end
})

function clip.add()
  local s = selection()
  if clip.store[#clip.store] == s then
    return true
  else
    table.insert(clip.store, s)
    if clip.combine then
      awful.spawn.easy_async('xsel | xsel -ipbs', function() end)
    end

    print("Added '" .. s .. "'")
    while #clip.store >= clip.max do
      table.remove(clip.store, 1)
    end

    clip.sync_items()

    return true
  end
end

function clip.enable(widget)
  clip.menu = widget or awful.widget.launcher({
    image  = beautiful.clip_icon,
    resize = true,
    forced_height = 5,
    menu = clip.menu_items
  })

  clip.menu:buttons(awful.util.table.join(
    awful.button({ }, 1, function()
      clip.menu_items:toggle()
    end)
  ))

  return clip.menu
end

return clip
