(local lspinstaller (require :nvim-lsp-installer))
(local lspconfig (require :lspconfig))
(local util (require :lspconfig/util))
(local {: nmap} (require :floppy.utils))
(local lsp vim.lsp)
(local sign-define vim.fn.sign_define)
(local cmp (require :cmp_nvim_lsp))
(local lua_dev (require :lua-dev))

(fn setup-mappings []
  (nmap :gD lsp.buf.declaration)
  (nmap :<Leader>e vim.diagnostic.open_float))

(fn make-capabilities []
  (local capabilities
         (cmp.update_capabilities (lsp.protocol.make_client_capabilities)))
  (capabilities.offsetEncoding [:utf-8])
  (local item capabilities.textDocument.completion.completionItem)
  (item.snippetSupport true)
  (item.preselectSupport true)
  (item.insertReplaceSupport true)
  (item.labelDetailsSupport true)
  (item.deprecatedSupport true)
  (item.commitCharactersSupport true)
  (item.tagSupport [:valueSet [1]])
  (item.resolveSupport {:properties [:documentation
                                     :detail
                                     :additionalTextEdits]})
  capabilities)

(fn setup-diagnostics []
  (vim.diagnostic.config {:virtual_text true
                          :signs true
                          :underline true
                          :update_in_insert false
                          :severity_sort false}))

(fn define-signs []
  (sign-define :LspDiagnosticsSignError
               {:text " " :texthl :LspDiagnosticsSignError})
  (sign-define :LspDiagnosticsSignWarning
               {:text " " :texthl :LspDiagnosticsSignWarning})
  (sign-define :LspDiagnosticsSignInformation
               {:text " " :texthl :LspDiagnosticsSignInformation})
  (sign-define :LspDiagnosticsSignHint
               {:text "" :texthl :LspDiagnosticsSignHint}))

(fn format-async [err result ctx]
  (if (and (not err) result)
      (do
        (local bufnr ctx.bufnr)
        (if (not (vim.api.nvim_buf_get_option bufnr :modified))
            (do
              (local view (vim.fn.winsaveview))
              (vim.lsp.util.apply_text_edits result bufnr :utf-16)
              (vim.fn.winrestview view)
              (if (= bufnr (vim.api.nvim_get_current_buf))
                  (vim.cmd "noautocmd :update")))))))

(tset vim.lsp.handlers :textDocument/formatting format-async)

(local capabilities
       (let [cmp (require :cmp_nvim_lsp)]
         (cmp.update_capabilities (vim.lsp.protocol.make_client_capabilities))))

(fn disable-formatting [client]
  (let [caps client.resolved_capabilities]
    (set caps.document_formatting false)
    (set caps.document_range_formatting false)))

(fn setup-clangd []
  (lspconfig.clangd.setup {:cmd [:clangd
                                 :--background-index
                                 :--compile-commands-dir
                                 :build]
                           :init_options {:compilationDatabasePath :build}
                           :root_dir (util.root_pattern :build/compile_commands.json)
                           :capabilities.offsetEncoding [:utf-16]
                           :on_attach disable-formatting}))

(fn setup-jdtls []
  (lspconfig.jdtls.setup {:root_dir (util.root_pattern :.git :pom.xml)
                          :on_attach disable-formatting}))

(fn setup-rust-analyzer []
  (lspconfig.rust_analyzer.setup {:settings {:rust-analyzer {:checkOnSave {:enabled true
                                                                           :command :clippy}}}}))

(fn setup-sumneko-lua []
  (set lspconfig.sumneko_lua.setup lua_dev.setup))

(fn setup-texlab []
  (lspconfig.texlab.setup {:on_attach disable-formatting}))

(fn setup-pyright []
  (lspconfig.pyright.setup {}))

(fn setup-servers []
  (lspinstaller.setup {:ensure_installed [:clangd
                                          :jdtls
                                          :rust_analyzer
                                          :sumneko_lua
                                          :texlab
                                          :pyright]})
  (setup-clangd)
  (setup-jdtls)
  (setup-rust-analyzer)
  (setup-sumneko-lua)
  (setup-texlab)
  (setup-pyright))

(fn setup []
  (setup-servers)
  (setup-mappings)
  (setup-diagnostics)
  (define-signs))

{: setup}
