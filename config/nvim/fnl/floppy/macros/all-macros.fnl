(import-macros {: tbl? : nil? : str? : ->str : odd? : even?}
               :floppy.macros.core-macros)

(fn setup! [name]
  `(let [module# (require ,name)]
     (module#.setup)))

(fn cfg! [name ...]
  `(fn []
     (let [module# (require ,name)]
       (module#.setup ,...))))

(fn head [xs]
  (. xs 1))

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
  `(.. (vim.fn.stdpath :data) "/" ,...))

(lambda parse-sym [xs]
  "parses symbol 'xs' converts it to string if not a variable."
  (if (or (in-scope? xs) (not (sym? xs)))
      (do
        xs)
      (tostring xs)))

(lambda hi! [name colors ?attributes]
  "Defines a highlight group using the vim API.
  e.g. (highlight! Error {:guifg \"#ff0000\"}) [:bold]"
  (assert-compile (sym? name) "expected symbol for name" name)
  (assert-compile (tbl? colors) "expected colors for colors" colors)
  (assert-compile (or (nil? ?attributes) (tbl? ?attributes))
                  "expected table for attributes" ?attributes)
  (let [name (->str name)
        colors (collect [_ v (ipairs (or ?attributes [])) :into colors]
                 (->str v)
                 true)]
    `(vim.api.nvim_set_hl 0 ,name ,colors)))

(lambda g! [name val]
  "sets global variable 'name' to 'val'."
  `(tset vim.g ,(parse-sym name) ,val))

(lambda b! [name val]
  "sets buffer scoped variable 'name' to 'val'."
  `(tset vim.b ,(parse-sym name) ,val))

(lambda set! [name ?val]
  "sets vim option 'name', optionally taking 'val'."
  (local name (parse-sym name))
  (if (not= nil ?val)
      `(tset vim.opt ,name ,?val)
      (str? name)
      (if (= :no (string.sub name 1 2))
          `(tset vim.opt ,(string.sub name 3) false)
          `(tset vim.opt ,name true)) ; else at runtime
      `(if (= :no (string.sub ,name 1 2))
           (tset vim.opt (string.sub ,name 3) false)
           (tset vim.opt ,name true))))

(lambda set+ [name val]
  "appends 'val' to vim option 'name'."
  `(: (. vim.opt ,(parse-sym name)) :append ,val))

(lambda set^ [name val]
  "prepends 'val' to vim option 'name'."
  `(: (. vim.opt ,(parse-sym name)) :prepend ,val))

(lambda rem! [name val]
  "removes 'val' from vim option 'name'."
  `(: (. vim.opt ,(parse-sym name)) :remove ,val))

(fn or= [x ...]
  "checks if 'x' is equal to any one of {...}"
  `(do
     (var out# false)
     (each [# v# (ipairs [,...])]
       (when (= ,x v#)
         (set out# true)
         (lua :break)))
     :return
     out#))

(lambda parse-list [sx]
  "parses symbols present in sequence 'sx'."
  (if (sequence? sx)
      (vim.tbl_map parse-sym sx)
      (parse-sym sx)))

(fn fn? [x]
  "Returns whether the parameter(s) is a function.
  A function is defined as any list with 'fn or 'hashfn as their first
  element."
  (and (list? x) (or (= `fn (head x)) (= `hashfn (head x)))))

(lambda quote? [x]
  "checks if 'x' is quoted value."
  (let [ref (?. x 1 1)]
    (= ref :quote)))

(lambda parse-cmd [xs ...]
  "parses command 'xs', wrapping it in function if quoted."
  (if (quote? xs)
      (let [ref (. xs 2)]
        (if (list? ref) `(fn []
                           ,ref) ref))
      :else
      xs))

(lambda parse-callback [cmd]
  "parses cmd into valid (name callback) chunk for opts in lua api."
  (if (or (fn? cmd) (quote? cmd))
      (values :callback (parse-cmd cmd))
      (values :command (do
                         cmd))))

(lambda autocmd! [events pattern cmd]
  "defines autocmd for group of 'id'."
  (local events (icollect [_ v (pairs events)]
                  (->str v)))
  ;; parse patterns
  (local pattern (if (sequence? pattern) (parse-list pattern)
                     (parse-sym pattern)))
  ;; parse callback
  (local (name val) (parse-callback cmd))
  :return
  `(vim.api.nvim_create_autocmd ,events {:pattern ,pattern ,name ,val}))

;; -------------------- ;;
;;        SETUP         ;;
;; -------------------- ;;
(lambda bootstrap []
  "installs packer in data dir if not already installed."
  (let [url "https://github.com/wbthomason/packer.nvim"
        path (.. (vim.fn.stdpath :data) :/site/pack/packer/start/packer.nvim)]
    `(when (= 0 (vim.fn.isdirectory ,path))
       (print "packer.nvim: installing in data dir...")
       (tset _G :packer_bootstrap
             (vim.fn.system [:git :clone :--depth :1 ,url ,path]))
       (vim.cmd :redraw)
       (vim.cmd "packadd packer.nvim")
       (print "packer.nvim: installed"))))

(fn packer-setup [opts]
  "bootstraps and setups config of packer with 'opts'."
  `(do
     ,(bootstrap)
     ((. (require :packer) :init) ,(or opts {}))))

;; -------------------- ;;
;;       STARTUP        ;;
;; -------------------- ;;
(lambda packer [...]
  "syntactic sugar over packer's startup function."
  (local packer `(require :packer))
  `((. ,packer :startup) (lambda [,(sym :use)]
                           (use :wbthomason/packer.nvim)
                           (do
                             ,...)
                           (if _G.packer_bootstrap
                               ((. ,packer :sync))))))

;; -------------------- ;;
;;         USE          ;;
;; -------------------- ;;
(lambda parse-conf [name opts]
  "parses 'name' and list of 'opts' into valid packer.use args."
  (local out [name])
  (each [idx val (ipairs opts)]
    (local nval (. opts (+ idx 1)))
    (if (odd? idx)
        (match val
          :module (tset out :config `#(require ,nval))
          _ (tset out val nval))))
  :return
  out)

(lambda use! [name ...]
  "syntactic sugar over packer's use function."
  (assert-compile (str? name) (.. "  packer-use: invalid name " (view name))
                  name)
  (assert-compile (even? (length [...]))
                  (.. "  packer-use: error in :" name
                      ", opts must contain even number of key-value pairs."))
  :return
  `(use ,(parse-conf name [...])))

{: setup!
 : cfg!
 : map!
 : nmap!
 : vmap!
 : xmap!
 : lazy!
 : datapath!
 : hi!
 : g!
 : b!
 : set!
 : set+
 : set^
 : rem!
 : autocmd!
 : or=
 : use!
 : packer
 : packer-setup}
