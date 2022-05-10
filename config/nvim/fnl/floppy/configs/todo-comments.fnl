(local tdc (require :todo-comments))
(import-macros {: nmap!} :dna.vim)

(nmap! :<Leader>td :<Cmd>TodoTelescope<CR>)

(fn setup []
  (tdc.setup {
  :signs true
  :sign_priority 1000
  :keywords {:TODO {:color "#e0af68"}}
  :highlight {
    :keyword :bg
    :pattern ".*<(KEYWORDS)\\s*"
    :exclude [:log :org]
  }
  :search {
    :pattern "\\b(KEYWORDS)\\b"
    :command :rg
    :args [
      :--color=never
      :--no-heading
      :--with-filename
      :--line-number
      :--column
      :--glob=!*.log
    ]
  }
  })
)

{: setup}
