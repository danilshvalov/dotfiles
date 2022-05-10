(local yanky (require :yanky))
(import-macros {: map! : nmap! : xmap! : lazy! : hi!} :floppy.macros)
(local cmd vim.cmd)

(fn setup-mappings []
  (nmap! :p (lazy! yanky.put :p false))
  (nmap! :P (lazy! yanky.put :P false))
  (xmap! :p (lazy! yanky.put :p false))
  (xmap! :P (lazy! yanky.put :P false))
  (nmap! :gp (lazy! yanky.put :gp false))
  (nmap! :gP (lazy! yanky.put :gP false))
  (nmap! :gp (lazy! yanky.put :gp false))
  (nmap! :gP (lazy! yanky.put :gP false)))

(fn setup-highlights []
  (hi! YankyPut {:fg :black :bg "#ff9e64"})
  (hi! YankyYanked {:fg :black :bg "#ff9e64"}))

(fn setup []
  (yanky.setup {:highlight {:on_put true :on_yank true :timer 150}})
  (setup-mappings)
  (setup-highlights))

{: setup}
