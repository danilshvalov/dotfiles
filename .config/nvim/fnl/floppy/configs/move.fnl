(import-macros {: nmap! : vmap!} :floppy.macros)
(local {: MoveLine : MoveBlock : MoveHChar : MoveHBlock} (require :move))

(fn setup-mappings []
  (nmap! :<A-Up> (partial MoveLine -1))
  (nmap! :<A-l> (partial MoveHChar 1))
  (nmap! :<A-Down> (partial MoveLine 1))
  (vmap! :<A-Up> ":MoveBlock(-1)<CR>")
  (vmap! :<A-Down> ":MoveBlock(1)<CR>")
  (nmap! :<A-Left> (partial MoveHChar -1))
  (nmap! :<A-Right> (partial MoveHChar 1))
  (nmap! :<A-k> (partial MoveLine -1))
  (nmap! :<A-j> (partial MoveLine 1))
  (vmap! :<A-k> ":MoveBlock(-1)<CR>")
  (vmap! :<A-j> ":MoveBlock(1)<CR>")
  (nmap! :<A-h> (partial MoveHChar -1)))

(fn setup []
  (setup-mappings))

{: setup}
