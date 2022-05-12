(local ls (require :luasnip))
(local {: fmt} (require :luasnip.extras.fmt))
(local {
  :snippet             snip
  :snippet_node        node
  :indent_snippet_node isn
  :text_node           text
  :insert_node         ins
  :function_node       func
  :choice_node         choice
  :dynamic_node        dyn
  :restore_node        restore
} ls)

; TODO: remove below
; (local {: rep} (require :luasnip.extras))
; (local events (require :luasnip.util.events))
; (local ai (require :luasnip.nodes.absolute_indexer))
; (local util (require :luasnip.util.util))
; (local node_util (require :luasnip.nodes.util))

(macro add-postfix-snip [name]
  (local name (tostring name))
  `(ls.add_snippets "python" [
     (snip {:trig ,(.. "([\"'].+[\"'])%." name) :regTrig true :hidden true}
       (func #(.. ,name "(" (. $2.captures 1) ")") [])
     )
     (snip {:trig ,(.. "(%S+)%." name) :regTrig true :hidden true}
       (func #(.. ,name "(" (. $2.captures 1) ")") [])
     )
   ])
)


(add-postfix-snip print)
(add-postfix-snip len)

(ls.add_snippets "python" [
  (snip "init" [
   (text "def __init__(self")
   (ins 1)
   (text ["):" "\t"])
  ])
  (snip "for" [
     (text "for ")
     (ins 1)
     (text " in ")
     (ins 2)
     (text [":" "\t"])
  ])
  (snip "forr" [
     (text "for ")
     (ins 1)
     (text " in range(")
     (ins 2)
     (text ["):" "\t"])
  ])
  (snip "cls" [
     (text "class ")
     (ins 1)
     (text [":" "\t"])
     (text "def __init__(self")
     (ins 2)
     (text ["):" "\t\t"])
  ])
])
