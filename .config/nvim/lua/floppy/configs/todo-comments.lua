-- :fennel:1651221107
local tdc = require("todo-comments")
vim.keymap.set("n", "<Leader>td", "<Cmd>TodoTelescope<CR>", {silent = true})
local function setup()
  return tdc.setup({signs = true, sign_priority = 1000, keywords = {TODO = {color = "#e0af68"}}, highlight = {keyword = "bg", pattern = ".*<(KEYWORDS)\\s*", exclude = {"log"}}, search = {pattern = "\\b(KEYWORDS)\\b", command = "rg", args = {"--color=never", "--no-heading", "--with-filename", "--line-number", "--column", "--glob=!*.log"}}})
end
return {setup = setup}