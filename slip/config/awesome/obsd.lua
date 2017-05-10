local awful = require("awful")
local beautiful = require("beautiful")
local gears = require("gears")
local naughty = require("naughty")
local wibox = require("wibox")
local https = require("ssl.https")
local ltn12 = require("ltn12")

local batt_timer = gears.timer({ timeout = 1.5 })
local snap_timer = gears.timer({ timeout = 1.5 })
local vol_timer = gears.timer({ timeout = 1.5 })

local obsd = {}

obsd.enable_debug = false
obsd.current_volume = 0
obsd.battery_percent = 100
obsd.battery_charging = false

function obsd.debug(msg)
    if (obsd.debug) then
        print("obsd: " .. msg)
    end
end

--- Updates obsd.current_volume to the output of 'mixerctl outputs.master'.
function obsd.update_volume()
    awful.spawn.easy_async([[sh -c 'mixerctl outputs.master | cut -d, -f2']], function(stdout, stderr, reason, exit_code)
        stdout = stdout:gsub("%s+", "")
        obsd.current_volume = tonumber(stdout)
        print("slider value", obsd.volume_slider.value)
        obsd.volume_slider.value = obsd.current_volume
        print("current value", obsd.current_volume)
        obsd.debug("current_volume is: " .. tostring(obsd.current_volume))
    end)
    return true
end

--- Increments outputs.master by -30
function obsd.vol_down()
    awful.util.spawn([[sh -c 'mixerctl outputs.master=$(($(mixerctl outputs.master | cut -d, -f2)-30))']])
    obsd.debug("volume decreased")
end

--- Increments outputs.master by 30
function obsd.vol_up()
    awful.util.spawn([[ sh -c 'mixerctl outputs.master=$(($(mixerctl outputs.master | cut -d, -f2)+30))']])
    obsd.debug("volume increased")
end

--- Sets outputs.master.mute to on
function obsd.mute()
    awful.util.spawn('mixerctl outputs.master.mute on')
    obsd.debug("mute enabled")
end

--- Sets outputs.master.mute to off
function obsd.unmute()
    awful.util.spawn('mixerctl outputs.master.mute off')
    obsd.debug("mute disabled")
end

--- Sets 'obsd.battery_percent' to the value of `apm -l`
-- @see obsd.battery_percent
obsd.update_batt_percent = function()
    awful.spawn.easy_async('apm -l', function(stdout, stderr, reason, exit_code)
        stdout = stdout:gsub("%s+", "")
        obsd.battery_percent = tonumber(stdout)
        obsd.debug("battery_percent set to " .. stdout)
    end)
end

obsd.update_batt_charge = function()
    awful.spawn.easy_async('apm -a', function(stdout, stderr, reason, exit_code)
        stdout = stdout:gsub("%s+", "")
        obsd.debug("battery_charge set to " .. stdout)
        if (stdout == "0") then
            obsd.battery_charging = false
        else
            obsd.battery_charging = true
       end
    end)
end

obsd.volume_slider = wibox.widget {
    bar_shape           = gears.shape.rounded_rect,
    bar_height          = 3,
    bar_color           = beautiful.border_color,
    forced_width        = 30,
    handle_color        = beautiful.bg_normal,
    handle_shape        = gears.shape.circle,
    handle_border_color = beautiful.border_color,
    handle_border_width = 1,
    value               = obsd.current_volume,
    minimum             = 0,
    maximum             = 255,
    widget              = wibox.widget.slider,
}

obsd.battery_bar = wibox.widget {
    {
        background_color = beautiful.battery_bg,
        border_color     = beautiful.battery_border,
        border_width     = 1,
        forced_height    = 3,
        forced_width     = 80,
        id               = "batt_pct",
        max_value        = 1,
        shape            = gears.shape.octogon,
        step             = 1,
        value            = obsd.battery_percent / 100,
        widget           = wibox.widget.progressbar,
        margins = {
            top    = 2,
            left   = 4,
            bottom = 2,
        },
    },
    {
        align  = 'center',
        id     = "batt_ac",
        text   = '',
        widget = wibox.widget.textbox,
    },
    layout = wibox.layout.stack,
}

obsd.snap_found = false
obsd.snap_version = ""

obsd.snap_checkbox = wibox.widget {
    checked       = false,
    color         = beautiful.snap_new,
    border_color  = beautiful.snap_border,
    paddings      = 3,
    shape         = gears.shape.circle,
    widget        = wibox.widget.checkbox
}

function split(s, delimiter)
   result = {};
   for match in (s..delimiter):gmatch("(.-)"..delimiter) do
      table.insert(result, match);
   end
   return result;
end

obsd.update_snap = function()
    if obsd.snap_found then
        obsd.debug("snap already found, skipping")
        return true
    end

    local file = os.getenv("HOME") .. '/.last_snap'

    awful.spawn.easy_async('cat ' .. file, function(stdout, stderr, reason, exit_code)
        local t = {}
        local body, code, headers, status  = https.request{
            url = "https://ftp3.usa.openbsd.org/pub/OpenBSD/snapshots/amd64/BUILDINFO",
            sink = ltn12.sink.table(t),
        }

        nt = split(t[1], "-")

        stdout = stdout:gsub("%s+$", "")
        a, _ = stdout:gsub("^%s*(.-)%s*$", "%1")
        b, _ = nt[2]:gsub("^%s*(.-)%s*$", "%1")

        if (a == b) then
            obsd.snap_checkbox.checked = false
            obsd.snap_version = tostring(b)
            obsd.debug("no new snapshots")
        else
            obsd.snap_checkbox.checked = true
            obsd.debug("new snapshots!")
            obsd.snap_found = true
            obsd.snap_version = tostring(a)
            naughty.notify({
                preset = naughty.config.presets.normal,
                title = "New OpenBSD snapshot!",
                text = tostring(a),
            })
        end
    end)
    return true
end

obsd.update_battery = function()
    obsd.update_batt_percent()
    obsd.update_batt_charge()

    local p = obsd.battery_percent
    if (p > 50) then
        obsd.battery_bar.batt_pct.color = beautiful.battery_high
    end
    if (p < 50 and p > 20) then
        obsd.battery_bar.batt_pct.color = beautiful.battery_medium
    end
    if (p < 20) then
        obsd.battery_bar.batt_pct.color = beautiful.battery_low
    end

    obsd.battery_bar.batt_pct.value = p / 100

    if (obsd.battery_charging) then
        obsd.battery_bar.batt_ac:set_markup('<span color="#000">⚡ </span>')
        if (p < 49) then
             obsd.battery_bar.batt_ac:set_markup('<span color="#000">⚡ </span>')
        end
    else
        if (p < 10) then
             obsd.battery_bar.batt_ac:set_markup('<span color="#000">🔌 </span>')
        else
             obsd.battery_bar.batt_ac:set_markup('<span color="#000"></span>')
        end
    end

    return true
end

obsd.battery_notification = function()
    obsd.update_battery()
    awful.spawn.easy_async('apm', function(stdout, stderr, reason, exit_code)
        naughty.notify({
            preset = naughty.config.presets.normal,
            title = "Power Status",
            text = tostring(stdout),
        })
    end)
end

obsd.battery_bar:buttons(awful.util.table.join(awful.button({ }, 1, obsd.battery_notification)))
obsd.snap_checkbox:buttons(awful.util.table.join(awful.button({ }, 1, function()
    obsd.update_snap()
    naughty.notify({
        preset = naughty.config.presets.normal,
        title = "Current Snap Date",
        text = obsd.snap_version,
    })
end)))

obsd.update_battery()
obsd.update_snap()
obsd.update_volume()

snap_timer.start_new(3600, obsd.update_snap)
vol_timer.start_new(5, obsd.update_volume)
batt_timer.start_new(5, obsd.update_battery)

return obsd

-- vim: tabstop=8 expandtab shiftwidth=4 softtabstop=4