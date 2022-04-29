(local treesitter (require :nvim-treesitter.configs))

(set vim.o.foldmethod :expr)
(set vim.o.foldlevel 20)
(set vim.o.foldexpr "nvim_treesitter#foldexpr()")

(fn setup []
  (treesitter.setup {:ensure_installed [:cpp
                                        :python
                                        :json
                                        :rust
                                        :java
                                        :lua
                                        :html
                                        :latex]
                     :highlight {:enable true
                                 :disable [:html :latex :org]
                                 :additional_vim_regex_highlighting [:org]}
                     :indent {:enable false}
                     :rainbow {:colors ["#00a960"
                                        "#e6b422"
                                        "#7aa2f7"
                                        "#8080ff"
                                        "#0073a8"
                                        "#33ccff"
                                        "#8080ff"]
                               :enable true
                               :extended_mode true
                               :max_file_lines 2500}
                     ":autopairs" {:enable true}}))

{: setup}
