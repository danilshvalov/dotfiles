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

(macro env-snip [trig name]
  (let [trig (tostring trig) name (tostring name)]
    `(snip ,trig [
       (text [(.. "\\begin{" ,name "}") "\t"])
       (ins 0)
       (text ["" (.. "\\end{" ,name "}")])
    ])
  )
)

(macro text-snip [trig name]
  `(snip ,(tostring trig) (text ,(tostring name)))
)


(macro sec-snip [trig name prefix]
  `(snip ,(tostring trig) [
    (text (.. ,(tostring name) "{"))
    (ins 1)
    (text ["}" (.. "\\label{" ,(tostring prefix) ":")])
    (rep 1)
    (text ["}" ""])
  ])
)

(macro wrap-snip [trig name]
  `(snip {:trig ,(tostring trig) :wordTrig false} [(text (.. :\ ,(tostring name) "{")) (ins 1) (text "}")])
)


(ls.add_snippets "tex" [
  (env-snip eq equation)
  (env-snip eqs equation*)
  (env-snip item itemize)
  (env-snip enum enumerate)
  (env-snip side siderules)
  (env-snip the  theorem)
  (env-snip thes theorem*)
  (env-snip pro  proof)
  (env-snip prop property)
  (env-snip gat  gather)
  (env-snip gats gather*)
  (env-snip gatd gathered)
  (env-snip cas dcases*)
  (env-snip cen center)
  (env-snip long longtable)

  (sec-snip sec   \section        sec)
  (sec-snip secs  \section*       sec)
  (sec-snip sub   \subsection     sub)
  (sec-snip subs  \subsection*    sub)
  (sec-snip ssub  \subsubsection  ssub)
  (sec-snip ssubs \subsubsection* ssub)


  (text-snip \a \alpha)
  (text-snip \b \beta)
  (text-snip \g \gamma)
  (text-snip \l \lambda)
  (text-snip \p \partial)
  (text-snip \D \Delta)
  (text-snip \i "\\item ")
  (text-snip \vep \varepsilon)
  (text-snip \vphi \varphi)
  (text-snip -> \to)
  (text-snip => \implies)
  (text-snip \inf \infty)
  (text-snip <= \leq)
  (text-snip >= \geq)
  (text-snip != \neq)
  (text-snip ?= \approx)
  (text-snip === \equiv)
  (text-snip * \cdot)
  (text-snip \q \quad)
  (text-snip \qq \qquad)
  (text-snip \qqq "\\qquad \\qquad")

  (wrap-snip \t text)
  (wrap-snip \s sqrt)
])
