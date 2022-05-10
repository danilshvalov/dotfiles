(import-macros {: odd? : str?} :dna.core)
(import-macros {: hi! : set!} :dna.vim)
(local lualine (require :lualine))

(macro str [x] `(fn [] ,x))

(macro item [kind ...]
  (local opts [...])
  (local out [kind])
  (each [idx val (ipairs opts)]
    (local nval (. opts (+ idx 1)))
    (if `(odd? ,idx) (tset out val nval))
  )
  out
)


(local conditions {
    :buffer_not_empty #(not= (vim.fn.empty (vim.fn.expand "%:t")) 1)
    :check_git_workspace (fn []
      (local filepath (vim.fn.expand "%:p:h"))
      (local gitdir (vim.fn.finddir ".git" (.. filepath ";")))
      (and gitdir (> (length gitdir) 0) (< (length gitdir) (length filepath)))
    )
})

(local sections {
  :lualine_a [
    (item
      (str " ")
      :padding {:right 0}
    )
  ]
  :lualine_b [(item :mode)]
  :lualine_c [
    (item
      (str "%F")
      :cond conditions.buffer_not_empty
    )
  ]
  :lualine_x [
    (item :progress)
    (item
      #(vim.opt.tabstop:get)
      :icon "הּ"
      :cond conditions.buffer_not_empty
    )
    (item
      "o:encoding"
      :fmt string.upper
      :cond conditions.buffer_not_empty
    )
    (item
      :filetype
      :icons_enabled false
    )
  ]
  :lualine_y [
    (item
      "branch"
      :icon :
      :cond conditions.check_git_workspace
    )
  ]
  :lualine_z [
    (item
      (str " ")
      :padding {:left 0}
    )
  ]
})

(set! noshowmode)
(lualine.setup {
  :options {
    :theme :auto
    :globalstatus true
    :component_separators ""
    :section_separators ""
  }
  : sections
  :inactive_item sections
})

; TODO: remove
(fn setup [])

{: setup}
