(local cmp (require :cmp))
(local luasnip (require :luasnip))
(local api vim.api)

(local icons {:Text ""
              :Method ""
              :Function ""
              :Constructor "⌘"
              :Field "ﰠ"
              :Variable ""
              :Class "ﴯ"
              :Interface ""
              :Module ""
              :Property "ﰠ"
              :Unit "塞"
              :Value ""
              :Enum ""
              :Keyword "廓"
              :Snippet ""
              :Color ""
              :File ""
              :Reference ""
              :Folder ""
              :EnumMember ""
              :Constant ""
              :Struct "פּ"
              :Event ""
              :Operator ""
              :TypeParameter ""})

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

(fn setup-mappings []
  {:<C-d> (map-scroll-docs -4)
   :<C-f> (map-scroll-docs 4)
   :<C-Space> (cmp.mapping (cmp.mapping.complete) [:i :c])
   :<C-n> (cmp.mapping (cmp.mapping.complete) [:i :c])
   :<Up> (cmp.mapping (cmp.mapping.select_prev_item) [:i :c :s])
   :<Down> (cmp.mapping (cmp.mapping.select_next_item) [:i :c :s])
   :<C-y> cmp.config.disable
   :<C-e> (cmp.mapping {:i (cmp.mapping.abort) :c (cmp.mapping.close)})
   :<CR> (cmp.mapping.confirm {:select false})
   :<Tab> (cmp.mapping {:i (fn [fallback]
                             (if (cmp.get_selected_entry) (cmp.confirm)
                                 (luasnip.expand_or_jumpable) (luasnip.expand_or_jump)
                                 (fallback)))
                        :s (fn [fallback]
                             (if (luasnip.jumpable 1)
                                 (luasnip.jump 1)
                                 (fallback)))
                        :c (cmp.mapping.select_next_item)})
   :<S-Tab> (cmp.mapping {:i (fn [fallback]
                               (if (luasnip.jumpable -1)
                                   (luasnip.jump -1)
                                   (fallback)))
                          :s (fn [fallback]
                               (if (luasnip.jumpable -1)
                                   (luasnip.jump -1)
                                   (fallback)))
                          :c (cmp.mapping.select_prev_item)})})

(fn setup-sources []
  (local sources [:path
                  :orgmode
                  :nvim_lsp
                  :nvim_lsp_signature_help
                  :neorg
                  :luasnip])
  (icollect [index value (ipairs sources)]
    {:name value :priority index}))

(fn setup []
  (cmp.setup.cmdline ":"
                     {:sources (cmp.config.sources [{:name :path}]
                                                   [{:name :cmdline}])})
  (cmp.setup {:snippet {:expand (fn [args]
                                  (luasnip.lsp_expand args.body))}
              :mapping (setup-mappings)
              :sources (setup-sources)
              :formatting {:fields [:kind :abbr :menu]
                           :format (fn [_ vim_item]
                                     (set vim_item.menu vim_item.kind)
                                     (set vim_item.kind (. icons vim_item.kind))
                                     vim_item)}}))

{: setup}
