(local cmp (require :cmp))
(local snippy (require :snippy))
(local api vim.api)

(fn is-word [line col]
  (let [lines (api.nvim_buf_get_lines 0 (- line 1) line true)]
    (let [current_line (. lines 1)]
      (let [char (current_line:sub 0 0)]
        (not (char:match "%s"))))))

(fn has-words-before []
  (local (line col) (unpack (api.nvim_win_get_cursor 0)))
  (and (not= col 0) (is-word line col)))

(fn map-scroll-docs [n]
  (cmp.mapping (cmp.mapping.scroll_docs n) [:i :c]))

(fn setup-snippets []
  {:expand (fn [args]
             (snippy.expand_snippet args.body))})

(fn setup-mappings []
  {:<C-d> (map-scroll-docs -4)
   :<C-f> (map-scroll-docs 4)
   :<C-Space> (cmp.mapping (cmp.mapping.complete) [:i :c])
   :<Up> (cmp.mapping (cmp.mapping.select_prev_item {:behavior cmp.SelectBehavior.Select})
                      [:i])
   :<Down> (cmp.mapping (cmp.mapping.select_next_item {:behavior cmp.SelectBehavior.Select})
                        [:i])
   :<C-y> cmp.config.disable
   :<C-e> (cmp.mapping {:i (cmp.mapping.abort) :c (cmp.mapping.close)})
   :<CR> (cmp.mapping.confirm {:select false})
   :<Tab> (cmp.mapping (fn [fallback]
                         (if (snippy.can_expand_or_advance)
                             (snippy.expand_or_advance)
                             (cmp.visible)
                             (cmp.select_next_item)
                             (fallback))) [:i :c :s])
   :<S-Tab> (cmp.mapping (fn [fallback]
                           (if (cmp.visible) (cmp.select_prev_item)
                               (snippy.can_jump -1) (snippy.previous)
                               (fallback))) [:i :c :s])})

(fn setup-sources []
  (local sources [:path :orgmode :nvim_lsp :nvim_lsp_signature_help :snippy])
  (icollect [index value (ipairs sources)]
    {:name value :priority index}))

(fn setup []
  (cmp.setup.cmdline ":"
                     {:sources (cmp.config.sources [{:name :path}]
                                                   [{:name :cmdline}])})
  (cmp.setup {:snippet (setup-snippets)
              :mapping (setup-mappings)
              :sources (setup-sources)}))

{: setup}
