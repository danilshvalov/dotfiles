(import-macros {: map! : command!} :dna.vim)
(local null_ls (require :null-ls))
(local api vim.api)

(var enable_format true)
(fn toggle-format []
  (set enable_format (not enable_format)))

(command! :ToggleFormat toggle-format)
(map! :<Leader>tf toggle-format)

(fn make-sources []
  (local builtins null_ls.builtins)
  (local formatting builtins.formatting)
  [builtins.code_actions.gitsigns
   builtins.diagnostics.eslint
   builtins.diagnostics.codespell
   formatting.stylua
   (formatting.prettier.with {:filetypes [:java :markdown]})
   (formatting.prettierd.with {:filetypes [:json :javascript]})
   formatting.clang_format
   formatting.black
   (formatting.latexindent.with {:extra_args ["-g /dev/null"]})
   formatting.taplo
   ;formatting.fnlfmt
  ])


(fn lsp-formatting [bufnr]
  (vim.lsp.buf.format {:async true}))

(fn format-file []
  (if enable_format (lsp-formatting)))

(local group_id (api.nvim_create_augroup :LspFormatting []))

(fn on-attach [client bufnr]
  (when (client.supports_method :textDocument/formatting)
    (map! :<F3> lsp-formatting)
    (api.nvim_clear_autocmds {:group group_id :buffer bufnr})
    (api.nvim_create_autocmd [:BufWritePost]
                             {:group group_id
                             :callback format-file
                             :buffer bufnr})))

(fn setup []
  (null_ls.setup {:sources (make-sources) :on_attach on-attach}))

{: setup}
