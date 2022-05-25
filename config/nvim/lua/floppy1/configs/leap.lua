-- :fennel:1652600437
local leap = require("leap")
local function setup_highlights()
  vim.api.nvim_set_hl(0, "LeapMatch", {fg = "#ff768e"})
  vim.api.nvim_set_hl(0, "LeapLabelPrimary", {bg = "#ff768e"})
  vim.api.nvim_set_hl(0, "LeapLabelSecondary", {bg = "#ff768e"})
  return vim.api.nvim_set_hl(0, "LeapBackdrop", {fg = "gray"})
end
local function setup()
  leap.setup({})
  leap.set_default_keymaps()
  return setup_highlights()
end
return {setup = setup}