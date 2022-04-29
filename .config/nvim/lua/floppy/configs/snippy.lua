-- :fennel:1651221107
local snippy = require("snippy")
local function setup()
  snippy.setup({})
  vim.g.snips_author = "Danil Shvalov"
  return nil
end
return {setup = setup}