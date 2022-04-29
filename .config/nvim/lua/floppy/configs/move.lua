-- :fennel:1651221107
local _local_1_ = require("move")
local MoveLine = _local_1_["MoveLine"]
local MoveBlock = _local_1_["MoveBlock"]
local MoveHChar = _local_1_["MoveHChar"]
local MoveHBlock = _local_1_["MoveHBlock"]
local function setup_mappings()
  local function _2_(...)
    return MoveLine(-1, ...)
  end
  vim.keymap.set("n", "<A-Up>", _2_, {silent = true})
  local function _3_(...)
    return MoveHChar(1, ...)
  end
  vim.keymap.set("n", "<A-l>", _3_, {silent = true})
  local function _4_(...)
    return MoveLine(1, ...)
  end
  vim.keymap.set("n", "<A-Down>", _4_, {silent = true})
  vim.keymap.set("v", "<A-Up>", ":MoveBlock(-1)<CR>", {silent = true})
  vim.keymap.set("v", "<A-Down>", ":MoveBlock(1)<CR>", {silent = true})
  local function _5_(...)
    return MoveHChar(-1, ...)
  end
  vim.keymap.set("n", "<A-Left>", _5_, {silent = true})
  local function _6_(...)
    return MoveHChar(1, ...)
  end
  vim.keymap.set("n", "<A-Right>", _6_, {silent = true})
  local function _7_(...)
    return MoveLine(-1, ...)
  end
  vim.keymap.set("n", "<A-k>", _7_, {silent = true})
  local function _8_(...)
    return MoveLine(1, ...)
  end
  vim.keymap.set("n", "<A-j>", _8_, {silent = true})
  vim.keymap.set("v", "<A-k>", ":MoveBlock(-1)<CR>", {silent = true})
  vim.keymap.set("v", "<A-j>", ":MoveBlock(1)<CR>", {silent = true})
  local function _9_(...)
    return MoveHChar(-1, ...)
  end
  return vim.keymap.set("n", "<A-h>", _9_, {silent = true})
end
local function setup()
  return setup_mappings()
end
return {setup = setup}