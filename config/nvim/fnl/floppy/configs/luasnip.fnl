(import-macros {: command! : keymap!} :dna.vim)

(local ls (require :luasnip))
(local snip ls.snippet)
(local node ls.snippet_node)
(local isn ls.indent_snippet_node)
(local text ls.text_node)
(local ins ls.insert_node)
(local func ls.function_node)
(local choice ls.choice_node)
(local dyn ls.dynamic_node)
(local restore ls.restore_node)
(local {: rep} (require :luasnip.extras))
(local events (require :luasnip.util.events))
(local ai (require :luasnip.nodes.absolute_indexer))
(local {: fmt} (require :luasnip.extras.fmt))
(local util (require :luasnip.util.util))
(local node_util (require :luasnip.nodes.util))
(local {:load load-snips} (require :luasnip.loaders.from_lua))
; (local m require("luasnip.extras").m
; (local lambda (require "luasnip.extras").l

(fn add-snippets [...]
  (ls.add_snippets "tex" [...]))


(fn text-snip [trig name]
  (snip trig (text name)))

(fn section-snips [prefix name]
  (local sections [
    {:trig :sec :name :\section :prefix :sec}
    {:trig :secs :name :\section* :prefix :sec}
    {:trig :sub :name :\subsection :prefix :sub}
    {:trig :subs :name :\subsection* :prefix :sub}
    {:trig :ssub :name :\subsubsection :prefix :ssub}
    {:trig :ssubs :name :\subsubsection* :prefix :ssub}
  ])

  (icollect [_ sec (pairs sections)]
    (let [{: trig : name : prefix} sec]
      (snip trig [
      (text (.. name "{"))
      (ins 1)
      (text ["}" (.. "\\label{" prefix ":")])
      (rep 1)
      (text ["}" ""])
      ])))
)



(fn setup-snippets []
  (ls.add_snippets "all" [(snip "--" (text "—"))])
  (ls.add_snippets "tex" [
    ; \Big()  -> \Big( \Big)
    ; \Bigg() -> \Bigg( \Bigg)
    ; \big()  -> \big( \big)
    ; \bigg() -> \bigg( \bigg)
    (snip {:trig "(\\[%B%b]ig?%g)([%(%[%{])" :regTrig true :hidden true}
      [
       (func (fn [args s] (.. (. s.captures 1) (. s.captures 2) " ")) {})
       (ins 1)
       (func (fn [args s] (.. " " (. s.captures 1))) {})
      ]
    )
    (snip :\beg (fmt "
                    \\begin{{{}}}
                      {}
                    \\end{{{}}}
                    "
                [(ins 1) (ins 0) (rep 1)])
    )
    (snip {:trig :mk :wordTrig true} (fmt "\\({}\\)" [(ins 1)]))
    (snip :\fr [(text "\\frac{") (ins 1) (text "}{") (ins 2) (text "}")])
    (snip :\dfr [(text "\\dfrac{") (ins 1) (text "}{") (ins 2) (text "}")])
    (snip :\lim [(text "\\lim_{") (ins 1) (text "}{") (ins 2) (text "}")])
    (snip :\int [(text "\\int\\limits_{") (ins 1) (text "}^{") (ins 2) (text "}")])
    (snip :\iint [(text "\\iint\\limits_{") (ins 1) (text "}")])
    (snip :\bo [(text "\\boxed{") (ins 1) (text "}")])
  ])
  ; (ls.add_snippets "tex" (section-snips))
  (ls.add_snippets "fennel" [
    (snip :req (fmt "(local {} (require :{}))" [(ins 1) (ins 2)]))
    (snip :im (fmt "(import-macros {{:{}}} :{})" [(ins 1) (ins 2)]))
  ])

  (ls.add_snippets "cpp" [
    (snip "main" (fmt "
                      int main() {{
                      \t{}
                      }}
                      " [(ins 0)]))
  ])
)

(fn reload-snippets []
  (ls.cleanup)
  (load-snips {:paths (.. (vim.fn.stdpath "config") "/lua/floppy/snippets")})
  (setup-snippets))


(fn setup []
  (ls.config.setup {
      :update_events "InsertLeave,TextChanged,TextChangedI"
      :region_check_events "CursorMoved,CursorHold,InsertEnter"
  })

  (load-snips {:paths (.. (vim.fn.stdpath "config") "/lua/floppy/snippets")})

  (setup-snippets)
  (command! "ReloadSnippets" reload-snippets)
)


{: setup}
