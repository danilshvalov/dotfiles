(local npairs (require :nvim-autopairs))
(local cond (require :nvim-autopairs.conds))
(local ts_conds (require :nvim-autopairs.ts-conds))
(local Rule (require :nvim-autopairs.rule))

(local ignore_regexp "[%w%.А-Я%а-яЁё%(%[%{%'%\"]")

(npairs.setup {:enable_check_bracket_line false
               :fast_wrap {:map :<A-x>
                           :pattern (string.gsub "[%'%\"%)%>%]%)%}%%s%^]" "%s+"
                                                 "")}
               :ignored_next_char (string.gsub ignore_regexp "%s+" "")
               :disable_filetype {}
               :enable_moveright false})

(npairs.add_rule (Rule "/*" "*/"))
(npairs.add_rule (Rule "r#\"" "\"#" :rust))
(npairs.add_rule (Rule "\\{" "\\}"))
(npairs.add_rule (Rule "\\(" "\\)"))
(npairs.add_rule (Rule "\\[" "\\]"))


(npairs.add_rule
  (doto (Rule "{" "};")
    (: :with_pair (fn [opts]
      (local struct (string.match opts.line "struct%s*%S*%s*$"))
      (local class (string.match opts.line "class%s*%S*%s*$"))
      (or (not= struct nil) (not= class nil))
    ))
  )
)

(npairs.add_rule
  (doto (Rule "<" ">")
    (: :with_pair #(not= (string.match $1.line "[%w%d]+$") nil))
  )
)



(fn setup [])

{: setup}
