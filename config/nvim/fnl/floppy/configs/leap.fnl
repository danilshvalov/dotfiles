(local leap (require :leap))
(import-macros {: hi!} :dna.vim)

(fn setup-highlights []
  (hi! LeapMatch {:fg "#ff768e"})
  (hi! LeapLabelPrimary {:bg "#ff768e"})
  (hi! LeapLabelSecondary {:bg "#ff768e"})
  (hi! LeapBackdrop {:fg :gray}))

(fn setup []
  (leap.setup {})
  (leap.set_default_keymaps)
  (setup-highlights)
)

{: setup}
