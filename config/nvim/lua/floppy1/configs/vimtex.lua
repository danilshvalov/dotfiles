-- :fennel:1651221107
local function setup()
  vim.g.vimtex_view_method = "skim"
  vim.g.vimtex_syntax_conceal_disable = true
  return nil
end
return {setup = setup}