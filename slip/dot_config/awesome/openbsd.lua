local openbsd = {}

function openbsd.batt_percent()
   return tonumber(io.popen('apm -l'):read())
end

function openbsd.charging()
   local apm = io.popen('apm -a'):read()
   if (apm == "0") then
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
