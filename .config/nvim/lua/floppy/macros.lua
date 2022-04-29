-- :fennel:1649744159
local function setup_21(name, ...)
  local mod = require(name)
  print(name)
  return mod.setup(...)
end
return {["setup!"] = setup_21}