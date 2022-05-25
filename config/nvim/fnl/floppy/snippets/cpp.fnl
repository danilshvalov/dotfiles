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


(ls.add_snippets "cpp" [
  (snip :inc [(text "#include <") (ins 1) (text :>)])
  (snip :std (text "using namespace std;"))
  (snip :cout [(text "std::cout << ") (ins 1) (text " << std::endl;")])
  (snip :main [(text ["int main() {" "\t"]) (ins 0) (text ["" "}"])])
  (snip :for
    (fmt
      "for ({} {} = {}; {}; {}) {{\n\t{}\n}}"
      [(ins 1 "size_t") (ins 2 "i") (ins 3 "0") (ins 4) (ins 5 "++i") (ins 0)]
    )
  )
])
