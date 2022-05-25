-- :fennel:1652349914
local indentline = require("indent_blankline")
local function setup()
  return indentline.setup({show_first_indent_level = true, show_trailing_blankline_indent = false, filetype_exclude = {"help", "terminal", "alpha", "packer", "lspinfo", "TelescopePrompt", "TelescopeResults", "nvchad_cheatsheet", "lsp-installer", ""}})
end
return {setup = setup}