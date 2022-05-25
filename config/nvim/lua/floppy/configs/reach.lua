-- :fennel:1652904536
local reach = require("reach")
local function _1_()
  return reach.buffers({handle = "dynamic"})
end
vim.keymap.set("n", "<C-X>", _1_, {silent = true})
vim.keymap.set("n", "<Leader>q", "<Cmd>Bdelete<CR>", {silent = true})
return reach.setup({notifications = false})