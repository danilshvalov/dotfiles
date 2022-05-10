(import-macros {: hi! : g!} :floppy.macros)

(fn setup-highlights []
  (g! conflict_marker_highlight_group "")
  (hi! ConflictMarkerBegin {:bg "#2f7366"})
  (hi! ConflictMarkerOurs {:bg "#2e5049"})
  (hi! ConflictMarkerTheirs {:bg "#344f69"})
  (hi! ConflictMarkerEnd {:bg "#2f628e"})
  (hi! ConflictMarkerCommonAncestorsHunk {:bg "#754a81"}))

(fn setup-markers []
  (g! conflict_marker_begin "^<<<<<<< .*$")
  (g! conflict_marker_end "^>>>>>>> .*$"))

(fn setup []
  (setup-markers)
  (setup-highlights))

{: setup}
