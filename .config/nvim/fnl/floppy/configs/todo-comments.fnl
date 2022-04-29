(local tdc (require :todo-comments))
(import-macros {: nmap!} :floppy.macros)

(nmap! :<Leader>td :<Cmd>TodoTelescope<CR>)

(fn setup []
  (tdc.setup {:signs true
              :sign_priority 1000
              :keywords {:TODO {:color "#e0af68"}}
              :highlight {:keyword :bg
                          :pattern ".*<(KEYWORDS)\\s*"
                          :exclude [:log]}
              :search {:pattern "\\b(KEYWORDS)\\b"
                       :command :rg
                       :args [:--color=never
                              :--no-heading
                              :--with-filename
                              :--line-number
                              :--column
                              :--glob=!*.log]}}))

{: setup}
