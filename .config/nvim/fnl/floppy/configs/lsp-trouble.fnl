(local trouble (require :trouble))
(import-macros {: nmap!} :floppy.macros)

(nmap! :<leader>xx :<cmd>TroubleToggle<cr>)

(fn setup []
  (trouble.setup {:icons true
                  :mode :workspace_diagnostics
                  :action_keys {:close :q
                                :cancel :<esc>
                                :refresh :r
                                :jump [:<cr> :<tab>]
                                :open_split [:<c-x>]
                                :open_vsplit [:<c-v>]
                                :open_tab [:<c-t>]
                                :jump_close [:o]
                                :toggle_mode :m
                                :toggle_preview :P
                                :hover :K
                                :preview :p
                                :close_folds [:zM :zm]
                                :open_folds [:zR :zr]
                                :toggle_fold [:zA :za]
                                :previous :k
                                :next :j}
                  :indent_lines true
                  :auto_open false
                  :auto_close false
                  :auto_preview true
                  :auto_fold false
                  :use_diagnostic_signs true}))

{: setup}
