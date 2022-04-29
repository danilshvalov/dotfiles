(local lualine (require :lualine))

(fn tab-size []
  (.. "tab: " (tostring vim.o.shiftwidth)))

(local sections {:lualine_a []
                 :lualine_b [:branch
                             [:diagnostics {:sources [:nvim_diagnostic]}]]
                 :lualine_c ["%F %m"]
                 :lualine_x [:encoding "%{&ff}" tab-size]
                 :lualine_y ["%2p%%"]
                 :lualine_z ["%3l:%-3c"]})

(fn setup []
  (lualine.setup {:options {:icons_enabled true
                            :component_separators {:left "" :right ""}
                            :always_divide_middle false
                            :section_separators {:left "" :right ""}
                            :disabled_filetypes [:neo-tree]
                            :globalstatus true}
                  : sections
                  :inactive_sections sections
                  :tabline {}
                  :extensions {}}))

{: setup}
