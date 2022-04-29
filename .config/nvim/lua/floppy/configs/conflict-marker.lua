-- :fennel:1651221107
local g = vim.g
local cmd = vim.cmd
local function setup_highlights()
  g.conflict_marker_highlight_group = ""
  cmd("highlight ConflictMarkerBegin guibg=#2f7366")
  cmd("highlight ConflictMarkerOurs guibg=#2e5049")
  cmd("highlight ConflictMarkerTheirs guibg=#344f69")
  cmd("highlight ConflictMarkerEnd guibg=#2f628e")
  return cmd("highlight ConflictMarkerCommonAncestorsHunk guibg=#754a81")
end
local function setup_markers()
  g.conflict_marker_begin = "^<<<<<<< .*$"
  g.conflict_marker_end = "^>>>>>>> .*$"
  return nil
end
local function setup()
  setup_markers()
  return setup_highlights()
end
return {setup = setup}