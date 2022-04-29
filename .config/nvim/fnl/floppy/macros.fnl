(fn setup! [name]
  `(let [module# (require ,name)]
     (module#.setup)))

(fn cfg! [name ...]
  `(fn []
     (let [module# (require ,name)]
       (module#.setup ,...))))

(fn make-opts [opts]
  (if (not opts) {:silent true} opts))

(fn keymap! [mode lhs rhs opts]
  (let [opts (make-opts opts)]
    `(vim.keymap.set ,mode ,lhs ,rhs ,opts)))

(fn map! [lhs rhs opts]
  (keymap! "" lhs rhs opts))

(fn nmap! [lhs rhs opts]
  (keymap! :n lhs rhs opts))

(fn vmap! [lhs rhs opts]
  (keymap! :v lhs rhs opts))

(fn xmap! [lhs rhs opts]
  (keymap! :x lhs rhs opts))

(fn double-eval-safe? [x type]
  (or (= :number type) (= :string type) (= :boolean type)
      (and (sym? x) (not (multi-sym? x)))))

(fn cmd [string]
  `(vim.cmd ,string))

(fn str? [x]
  (= :string (type x)))

(fn ->str [x]
  (tostring x))

(fn ->num [x]
  (tonumber x))

(fn lazy! [f ...]
  (assert f "expected a function to lazy apply")
  (let [bindings []
        args []]
    (each [_ arg (ipairs [...])]
      (if (double-eval-safe? arg (type arg))
          (table.insert args arg)
          (let [name (gensym)]
            (table.insert bindings name)
            (table.insert bindings arg)
            (table.insert args name))))
    (let [body (list f (unpack args))]
      (if (= 0 (length bindings))
          `(fn []
             ,body)
          `(let ,bindings
             (fn []
               ,body))))))

(fn datapath! [...]
  `(.. (vim.fn.stdpath :data) :/ ,...))

{: setup!
 : cfg!
 : map!
 : nmap!
 : vmap!
 : xmap!
 : lazy!
 : str?
 : ->str
 : ->num
 : datapath!}
