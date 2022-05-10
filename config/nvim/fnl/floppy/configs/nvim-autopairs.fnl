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
; (npairs.add_rule (doto (Rule "\\Big(" "\\Big)" :tex)
;                    (: :with_del (cond.none))))

; npairs.add_rule(Rule("<", ">", "cpp"):with_pair(ts_conds.is_not_ts_node({ "condition_clause", "repeat_statement" })))

; npairs.add_rules({
;     Rule(" ", " ")
;         :with_pair(function(opts)
;             local pair = opts.line:sub(opts.col - 1, opts.col)
;             return vim.tbl_contains({ "()", "[]", "{}" }, pair)
;         end)
;         :with_pair(cond.not_filetypes({ "org" })),
;     Rule("( ", " )")
;         :with_pair(function()
;             return false
;         end)
;         :with_move(function(opts)
;             return opts.prev_char:match(".%)") ~= nil
;         end)
;         :use_key(")")
;         :with_del(cond.none())
;         :with_pair(cond.not_filetypes({ "org" })),
;     Rule("{ ", " }")
;         :with_pair(function()
;             return false
;         end)
;         :with_move(function(opts)
;             return opts.prev_char:match(".%}") ~= nil
;         end)
;         :use_key("}")
;         :with_del(cond.none())
;         :with_pair(cond.not_filetypes({ "org" })),
;     Rule("[ ", " ]")
;         :with_pair(function()
;             return false
;         end)
;         :with_move(function(opts)
;             return opts.prev_char:match(".%]") ~= nil
;         end)
;         :use_key("]")
;         :with_del(cond.none())
;         :with_pair(cond.not_filetypes({ "org" })),
; })

(fn setup [])

{: setup}
