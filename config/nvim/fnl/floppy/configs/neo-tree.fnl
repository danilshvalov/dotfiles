(import-macros {: nmap! : hi! : autocmd!} :dna.vim)
(local neotree (require :neo-tree))
(local command (require :neo-tree.command))

(fn toggle-tree []
  (command.execute {:toggle true :position :right :source :filesystem}))

(fn setup []
  (neotree.setup {
    :filesystem {
      :window {:width 30 :position :right}
      :filtered_items {:hide_dotfiles false}
    }
    :event_handlers [
      {
        :event :file_opened
        :handler #(neotree.close_all)
      }
    ]
    :default_component_configs {
      :indent {
        :indent_size 2
          :padding 0
          :with_markers true
          :indent_marker "│"
          :last_indent_marker "└"
          :highlight :NeoTreeIndentMarker
          :with_expanders true
          :expander_collapsed ""
          :expander_expanded ""
          :expander_highlight :NeoTreeExpander
      }
    }
  })
  (nmap! :<C-t> toggle-tree)
  (hi! NeoTreeDirectoryIcon {:bg :NONE})
  (hi! NeoTreeTitleBar {:fg :black :bg :white})
)

{: setup}
