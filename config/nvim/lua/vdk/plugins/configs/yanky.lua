local yanky = require("yanky")
local utils = require("vdk.core.utils")
local nmap, xmap, hi = utils.nmap, utils.xmap, utils.hi

nmap("p", function() yanky.put("p", false) end)
nmap("P", function() yanky.put("P", false) end)
xmap("p", function() yanky.put("p", false) end)
xmap("P", function() yanky.put("P", false) end)
nmap("gp", function() yanky.put("gp", false) end)
nmap("gP", function() yanky.put("gP", false) end)
nmap("gp", function() yanky.put("gp", false) end)
nmap("gP", function() yanky.put("gP", false) end)

hi("YankyPut", {fg = "black", bg = "#ff9e64"})
hi("YankyYanked", {fg = "black", bg = "#ff9e64"})

yanky.setup({highlight = {on_put = true, on_yank = true, timer = 150}})
