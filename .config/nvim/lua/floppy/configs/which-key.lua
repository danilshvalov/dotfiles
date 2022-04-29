-- :fennel:1651221107
local wk = require("which-key")
local function setup()
  return wk.setup({plugins = {marks = true, registers = true, spelling = {enabled = false, suggestions = 20}, presets = {operators = true, motions = true, text_objects = true, windows = true, nav = true, z = true, g = true}}, icons = {breadcrumb = ">", separator = "->", group = "+"}, key_labels = {["<leader>"] = "SPC", ["<space>"] = "SPC"}})
end
return {setup = setup}