-- :fennel:1652169622
local neorg = require("neorg")
return neorg.setup({load = {["core.defaults"] = {}, ["core.keybinds"] = {}, ["core.norg.concealer"] = {}, ["core.norg.completion"] = {config = {engine = "nvim-cmp"}}}})