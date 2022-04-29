(local g vim.g)
(local cmd vim.cmd)

(fn setup-highlights []
  (set g.conflict_marker_highlight_group "")
  (cmd "highlight ConflictMarkerBegin guibg=#2f7366")
  (cmd "highlight ConflictMarkerOurs guibg=#2e5049")
  (cmd "highlight ConflictMarkerTheirs guibg=#344f69")
  (cmd "highlight ConflictMarkerEnd guibg=#2f628e")
  (cmd "highlight ConflictMarkerCommonAncestorsHunk guibg=#754a81"))

(fn setup-markers []
  (set g.conflict_marker_begin "^<<<<<<< .*$")
  (set g.conflict_marker_end "^>>>>>>> .*$"))

(fn setup []
  (setup-markers)
  (setup-highlights))

{: setup}
