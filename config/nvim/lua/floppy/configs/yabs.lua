-- :fennel:1651500698
local yabs = require("yabs")
local function setup()
  return yabs:setup({languages = {tex = {tasks = {run = {command = "make", output = "echo", type = "shell"}}}}})
end
local function _1_()
  return yabs:run_task("run")
end
vim.keymap.set("n", "<Leader>rt", _1_, {silent = true})
return {setup = setup}