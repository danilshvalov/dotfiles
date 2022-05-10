-- :fennel:1651251539
local yanky = require("yanky")
local cmd = vim.cmd
local function setup_mappings()
  local function _1_()
    return yanky.put("p", false)
  end
  vim.keymap.set("n", "p", _1_, {silent = true})
  local function _2_()
    return yanky.put("P", false)
  end
  vim.keymap.set("n", "P", _2_, {silent = true})
  local function _3_()
    return yanky.put("p", false)
  end
  vim.keymap.set("x", "p", _3_, {silent = true})
  local function _4_()
    return yanky.put("P", false)
  end
  vim.keymap.set("x", "P", _4_, {silent = true})
  local function _5_()
    return yanky.put("gp", false)
  end
  vim.keymap.set("n", "gp", _5_, {silent = true})
  local function _6_()
    return yanky.put("gP", false)
  end
  vim.keymap.set("n", "gP", _6_, {silent = true})
  local function _7_()
    return yanky.put("gp", false)
  end
  vim.keymap.set("n", "gp", _7_, {silent = true})
  local function _8_()
    return yanky.put("gP", false)
  end
  return vim.keymap.set("n", "gP", _8_, {silent = true})
end
local function setup_highlights()
  vim.api.nvim_set_hl(0, "YankyPut", {fg = "black", bg = "#ff9e64"})
  return vim.api.nvim_set_hl(0, "YankyYanked", {fg = "black", bg = "#ff9e64"})
end
local function setup()
  yanky.setup({highlight = {on_put = true, on_yank = true, timer = 150}})
  setup_mappings()
  return setup_highlights()
end
return {setup = setup}