-- :fennel:1651221107
local lualine = require("lualine")
local function tab_size()
  return ("tab: " .. tostring(vim.o.shiftwidth))
end
local sections = {lualine_a = {}, lualine_b = {"branch", {"diagnostics", {sources = {"nvim_diagnostic"}}}}, lualine_c = {"%F %m"}, lualine_x = {"encoding", "%{&ff}", tab_size}, lualine_y = {"%2p%%"}, lualine_z = {"%3l:%-3c"}}
local function setup()
  return lualine.setup({options = {icons_enabled = true, component_separators = {left = "", right = ""}, always_divide_middle = false, section_separators = {left = "", right = ""}, disabled_filetypes = {"neo-tree"}, globalstatus = true}, sections = sections, inactive_sections = sections, tabline = {}, extensions = {}})
end
return {setup = setup}