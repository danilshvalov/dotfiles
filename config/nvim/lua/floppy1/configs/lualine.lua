-- :fennel:1652903698
local lualine = require("lualine")
local conditions
local function _1_()
  return (vim.fn.empty(vim.fn.expand("%:t")) ~= 1)
end
local function _2_()
  local filepath = vim.fn.expand("%:p:h")
  local gitdir = vim.fn.finddir(".git", (filepath .. ";"))
  return (gitdir and (#gitdir > 0) and (#gitdir < #filepath))
end
conditions = {buffer_not_empty = _1_, check_git_workspace = _2_}
local sections
local function _3_()
  return " "
end
local function _4_()
  return "%F"
end
local function _5_()
  return (vim.opt.tabstop):get()
end
local function _6_()
  return " "
end
sections = {lualine_a = {{padding = {right = 0}, _3_}}, lualine_b = {{"mode"}}, lualine_c = {{cond = conditions.buffer_not_empty, _4_}}, lualine_x = {{"diagnostics"}, {"progress"}, {cond = conditions.buffer_not_empty, icon = "\239\172\180", ["\239\172\180"] = "cond", _5_}, {cond = conditions.buffer_not_empty, fmt = string.upper, [string.upper] = "cond", "o:encoding"}, {icons_enabled = false, "filetype"}}, lualine_y = {{cond = conditions.check_git_workspace, icon = "\238\156\165", ["\238\156\165"] = "cond", "branch"}}, lualine_z = {{padding = {left = 0}, _6_}}}
vim.opt["showmode"] = false
lualine.setup({options = {theme = "auto", globalstatus = true, component_separators = "", section_separators = ""}, sections = sections, inactive_item = sections})
local function setup()
end
return {setup = setup}