(local neorg (require :neorg))

(neorg.setup {
  :load {
    :core.defaults {}
    :core.keybinds {}
    :core.norg.concealer {}
    ; :core.presenter {}
    ; :core.gtd.base {}
    :core.norg.completion {
      :config {:engine :nvim-cmp}
    }
  }
})
