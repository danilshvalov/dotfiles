-- :fennel:1651927973
local lspsaga = require("lspsaga")
local diagnostic = require("lspsaga.diagnostic")
local code_action = require("lspsaga.codeaction")
local action = require("lspsaga.action")
local provider = require("lspsaga.provider")
local _local_1_ = require("lspsaga.rename")
local rename = _local_1_["rename"]
local _local_2_ = require("lspsaga.signaturehelp")
local signature_help = _local_2_["signature_help"]
local _local_3_ = require("lspsaga.hover")
local render_hover_doc = _local_3_["render_hover_doc"]
local function setup_mappings()
  vim.keymap.set("n", "<Leader>r", rename, {silent = true})
  vim.keymap.set("n", "<Leader>d", provider.preview_definition, {silent = true})
  vim.keymap.set("n", "<Leader>s", signature_help, {silent = true})
  vim.keymap.set("n", "<C-a>", code_action.code_action, {silent = true})
  vim.keymap.set("v", "<C-a>", code_action.range_code_action, {silent = true})
  vim.keymap.set("n", "<Leader>h", render_hover_doc, {silent = true})
  vim.keymap.set("n", "[e", diagnostic.navigate("next"), {silent = true})
  vim.keymap.set("n", "]e", diagnostic.navigate("prev"), {silent = true})
  vim.keymap.set("n", "<C-h>", diagnostic.show_line_diagnostics, {silent = true})
  local function _4_()
    return action.smart_scroll_with_saga(1)
  end
  vim.keymap.set("n", "<C-f>", _4_, {silent = true})
  local function _5_()
    return action.smart_scroll_with_saga(-1)
  end
  return vim.keymap.set("n", "<C-b>", _5_, {silent = true})
end
local function setup()
  lspsaga.setup({rename_prompt_prefix = "", code_action_prompt = {enable = false}})
  return setup_mappings()
end
return {setup = setup}