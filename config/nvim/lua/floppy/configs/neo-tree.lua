-- :fennel:1652084625
local neotree = require("neo-tree")
local command = require("neo-tree.command")
local function toggle_tree()
  return command.execute({toggle = true, position = "right", source = "filesystem"})
end
local function setup()
  local function _1_()
    return neotree.close_all()
  end
  neotree.setup({filesystem = {window = {width = 30, position = "right"}, filtered_items = {hide_dotfiles = false}}, event_handlers = {{event = "file_opened", handler = _1_}}, default_component_configs = {indent = {indent_size = 2, padding = 0, with_markers = true, indent_marker = "\226\148\130", last_indent_marker = "\226\148\148", highlight = "NeoTreeIndentMarker", with_expanders = true, expander_collapsed = "\239\145\160", expander_expanded = "\239\145\188", expander_highlight = "NeoTreeExpander"}}})
  vim.keymap.set("n", "<C-t>", toggle_tree, {silent = true})
  vim.api.nvim_set_hl(0, "NeoTreeDirectoryIcon", {bg = "NONE"})
  return vim.api.nvim_set_hl(0, "NeoTreeTitleBar", {fg = "black", bg = "white"})
end
return {setup = setup}