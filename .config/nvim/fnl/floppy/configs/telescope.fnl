(local telescope (require :telescope))
(local builtin (require :telescope.builtin))
(local actions (require :telescope.actions))
(local previewers (require :telescope.previewers))

(import-macros {: nmap!} :floppy.macros)

(nmap! :<Leader>ff (partial builtin.find_files {:hidden true :no_ignore true}))
(nmap! :<Leader>fr builtin.oldfiles)
(nmap! :<Leader>fg builtin.live_grep)
(nmap! :<Leader>fb builtin.buffers)
(nmap! :<Leader>fh builtin.help_tags)
(nmap! :<Leader>ls builtin.lsp_document_symbols)
(nmap! :<Leader>gc builtin.git_commits)
(nmap! :<Leader>gf builtin.git_files)
(nmap! :<Leader>gb builtin.git_branches)
(nmap! :<Leader>b builtin.buffers)
(nmap! :z= builtin.spell_suggest)
(nmap! :gr builtin.lsp_references)
(nmap! :gd builtin.lsp_definitions)
(nmap! :gi builtin.lsp_implementations)

(local default {:defaults {:find_files [:no_ignore]
                           :find_command [:rg
                                          :--no-heading
                                          :--hidden
                                          :--with-filename
                                          :--line-number
                                          :--column
                                          :--smart-case]
                           :prompt_prefix " "
                           :selection_caret " "
                           :entry_prefix " "
                           :initial_mode :insert
                           :selection_strategy :reset
                           :sorting_strategy :ascending
                           :layout_strategy :vertical
                           :layout_config {:vertical {:height 0.6
                                                      :prompt_position :top
                                                      :width 0.8
                                                      :mirror true
                                                      :preview_height 0.4}}
                           :path_display [:truncate]
                           :use_less true
                           :file_ignore_patterns [:node_modules/.* :.git/*]
                           :file_previewer previewers.vim_buffer_cat.new
                           :grep_previewer previewers.vim_buffer_vimgrep.new
                           :qflist_previewer previewers.vim_buffer_qflist.new
                           :buffer_previewer_maker previewers.buffer_previewer_maker
                           :mappings {:i {:<C-Down> actions.cycle_history_next
                                          :<C-Up> actions.cycle_history_prev}}
                           :history {:path "~/.local/share/nvim/databases/telescope_history.sqlite3"
                                     :limit 100}}})

(fn setup []
  (telescope.setup default)
  true)

{: setup}

; local extensions = {}

; for _, ext in ipairs(extensions) do
;     telescope.load_extension(ext)
; end
