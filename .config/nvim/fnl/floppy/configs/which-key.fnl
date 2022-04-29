(local wk (require :which-key))

(fn setup []
  (wk.setup {:plugins {:marks true
                       :registers true
                       :spelling {:enabled false :suggestions 20}
                       :presets {:operators true
                                 :motions true
                                 :text_objects true
                                 :windows true
                                 :nav true
                                 :z true
                                 :g true}}
             :icons {:breadcrumb ">" :separator "->" :group "+"}
             :key_labels {:<leader> :SPC :<space> :SPC}}))

{: setup}
