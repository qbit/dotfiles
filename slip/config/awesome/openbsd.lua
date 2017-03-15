local openbsd = {}

local read_data = function(cmd, amount)
	local a = io.popen(cmd)
	local info
	if (amount == nil) then
		info = a:read()
	else
		info = a:read(amount)
	end

	a:close()

	return info
end

function openbsd.batt_percent()
   local info = read_data('apm -l')
   return tonumber(info)
end

function openbsd.charging()
   local info = read_data('apm -a')
   if (info == "0") then
      return false
   else
      return true
   end
end

print(openbsd.batt_percent())
if (openbsd.charging()) then
   print("⚡")
else
   print(".")
end

return openbsd
