(local null_ls (require :null-ls))

(local api vim.api)
(import-macros {: map! : lazy! : str? : ->num} :floppy.macros)

(fn is-buffer-ready [bufnr]
  (and (api.nvim_buf_is_loaded bufnr)
       (not (api.nvim_buf_get_option bufnr :modified))))

(fn error-notify [kind err]
  (local message (or (and (str? err) err) err.message))
  (vim.notify (.. kind ": " message) vim.log.levels.WARN))

(fn update-buffer [bufnr res]
  (vim.lsp.util.apply_text_edits res bufnr :utf-16)
  (api.nvim_buf_call bufnr (lazy! vim.cmd "silent noautocmd update")))

(fn format-callback [bufnr err res]
  (if (and (is-buffer-ready bufnr) res)
      (update-buffer bufnr res)))

(fn parse-buffer-id [bufnr]
  (or (->num bufnr) (api.nvim_get_current_buf)))

(fn apply-format [bufnr]
  (let [bufnr (parse-buffer-id bufnr)]
    (vim.lsp.buf_request bufnr :textDocument/formatting
                         {:textDocument {:uri (vim.uri_from_bufnr bufnr)}}
                         (partial format-callback bufnr))))

(var enable_format true)

(fn toggle-format []
  (set enable_format (not enable_format)))

(api.nvim_create_user_command :ToggleFormat toggle-format [])
(map! :<Leader>tf toggle-format)

(fn make-sources []
  (local builtins null_ls.builtins)
  (local formatting builtins.formatting)
  [builtins.code_actions.gitsigns
   builtins.diagnostics.eslint
   formatting.stylua
   (formatting.prettier.with {:filetypes [:java :markdown]})
   (formatting.prettierd.with {:filetypes [:json :javascript]})
   formatting.clang_format
   formatting.black
   (formatting.latexindent.with {:extra_args ["-g /dev/null"]})
   formatting.taplo
   formatting.fnlfmt])

(fn format-file []
  (if enable_format (vim.lsp.buf.formatting)))

(fn on-attach [client bufnr]
  (if (client.supports_method :textDocument/formatting)
      (do
        (local group_id (api.nvim_create_augroup :LspFormatting []))
        (api.nvim_create_autocmd [:BufWritePost]
                                 {:group group_id :callback format-file}))))

(fn setup []
  (null_ls.setup {:sources (make-sources) :on_attach on-attach}))

{: setup}
