(local lsp_installer (require :nvim-lsp-installer))
(local util (require :lspconfig/util))

(local capabilities
       (let [cmp (require :cmp_nvim_lsp)]
         (cmp.update_capabilities (vim.lsp.protocol.make_client_capabilities))))

; local capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities())

(fn disable-formatting [client]
  (let [caps client.resolved_capabilities]
    (set caps.document_formatting false)
    (set caps.document_range_formatting false)))

(fn setup-clangd [opts]
  (set opts.cmd [:clangd :--background-index :--compile-commands-dir :build])
  (set opts.init_options {:compilationDatabasePath :build})
  (set opts.root_dir (util.root_pattern :build/compile_commands.json))
  (set opts.capabilities.offsetEncoding [:utf-16])
  (set opts.on_attach disable-formatting)
  opts)

(fn setup-jdtls [opts]
  (set opts.root_dir (util.root_pattern :.git :pom.xml))
  (set opts.on_attach disable-formatting)
  opts)

(fn setup-rust-analyzer [opts]
  (opts.settings {:rust-analyzer {:checkOnSave {:enabled true :command :clippy}}})
  opts)

(fn setup-sumneko-lua [opts]
  (let [lua_dev (require :lua-dev)]
    (lua_dev.setup)))

(fn setup-texlab [opts]
  (set opts.on_attach disable-formatting)
  opts)

(fn setup-server [server]
  (var opts {: capabilities})
  (if (= server.name :clangd) (set opts (setup-clangd opts)))
  (if (= server.name :jdtls) (set opts (setup-jdtls opts)))
  (if (= server.name :rust_analyzer) (set opts (setup-rust-analyzer opts)))
  (if (= server.name :sumneko_lua) (set opts (setup-sumneko-lua opts)))
  (if (= server.name :texlab) (set opts (setup-texlab opts)))
  (server:setup opts))

(fn setup []
  (lsp_installer.on_server_ready setup-server))

{: setup}

; lsp_installer.on_server_ready(function(server)
;     local opts = {
;         capabilities = capabilities,
;     }

;     if server.name == "clangd" then
;         opts.cmd = { "clangd", "--background-index", "--compile-commands-dir", "build" }
;         opts.init_options = {
;             compilationDatabasePath = "build",
;         }
;         opts.root_dir = util.root_pattern("build/compile_commands.json")

;         opts.on_attach = function(client)
;             client.resolved_capabilities.document_formatting = false
;             client.resolved_capabilities.document_range_formatting = false
;         end

;         capabilities.offsetEncoding = { "utf-16" }

;         opts.capabilities = capabilities
;     end

;     if server.name == "jdtls" then
;         opts.root_dir = require("lspconfig/util").root_pattern(".git", "pom.xml")

;         opts.on_attach = function(client)
;             client.resolved_capabilities.document_formatting = false
;             client.resolved_capabilities.document_range_formatting = false
;         end
;     end

;     if server.name == "rust_analyzer" then
;         opts.settings = {
;             ["rust-analyzer"] = {
;                 ["checkOnSave"] = {
;                     ["enabled"] = true,
;                     ["command"] = "clippy",
;                 },
;             },
;         }
;     end

;     if server.name == "sumneko_lua" then
;         opts = require("lua-dev").setup()
;     end

;     server:setup(opts)
; end)
