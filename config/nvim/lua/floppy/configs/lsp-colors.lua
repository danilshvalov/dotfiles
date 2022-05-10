-- :fennel:1651221107
local lspcolors = require("lsp-colors")
local function setup()
  return lspcolors.setup({Error = "#F44747", Warning = "#FF8800", Hint = "#4FC1FF", Information = "#FFCC66"})
end
return {setup = setup}