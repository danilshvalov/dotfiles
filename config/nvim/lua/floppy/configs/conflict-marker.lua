-- :fennel:1651251669
local function setup_highlights()
  vim.g["conflict_marker_highlight_group"] = ""
  vim.api.nvim_set_hl(0, "ConflictMarkerBegin", {bg = "#2f7366"})
  vim.api.nvim_set_hl(0, "ConflictMarkerOurs", {bg = "#2e5049"})
  vim.api.nvim_set_hl(0, "ConflictMarkerTheirs", {bg = "#344f69"})
  vim.api.nvim_set_hl(0, "ConflictMarkerEnd", {bg = "#2f628e"})
  return vim.api.nvim_set_hl(0, "ConflictMarkerCommonAncestorsHunk", {bg = "#754a81"})
end
local function setup_markers()
  vim.g["conflict_marker_begin"] = "^<<<<<<< .*$"
  vim.g["conflict_marker_end"] = "^>>>>>>> .*$"
  return nil
end
local function setup()
  setup_markers()
  return setup_highlights()
end
return {setup = setup}