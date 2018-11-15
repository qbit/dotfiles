local keys = {}
-- local naughty = require("naughty")

local function _get_return(cmd)
   local rt = false
   local code, _ = os.execute(cmd)
   if (code) then
      rt = true
   end
   return rt
end

function keys.ssh_loaded()
   return _get_return("ssh-add -L")
end

function keys.ssh_notify()
   if (keys.ssh_loaded()) then
      print("loaded")
      -- naughty.notify({
	    -- preset = naughty.config.presets.normal,
	    -- title = "ssh-agent status",
	    -- text = "Keys loaded.",
      -- })
   else
      print("unloaded")
      -- naughty.notify({
	    -- preset = naughty.config.presets.normal,
	    -- title = "ssh-agent status",
	    -- text = "No keys loaded.",
      -- })
   end
end

keys.ssh_notify()

return keys
