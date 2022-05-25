-- :fennel:1653322106
local lspinstaller = require("nvim-lsp-installer")
local lspconfig = require("lspconfig")
local util = require("lspconfig/util")
local _local_1_ = require("floppy.utils")
local nmap = _local_1_["nmap"]
local lsp = vim.lsp
local sign_define = vim.fn.sign_define
local cmp = require("cmp_nvim_lsp")
local lua_dev = require("lua-dev")
local function setup_mappings()
  nmap("gD", lsp.buf.declaration)
  return nmap("<Leader>e", vim.diagnostic.open_float)
end
local function make_capabilities()
  local capabilities = cmp.update_capabilities(lsp.protocol.make_client_capabilities())
  capabilities.offsetEncoding({"utf-8"})
  local item = capabilities.textDocument.completion.completionItem
  item.snippetSupport(true)
  item.preselectSupport(true)
  item.insertReplaceSupport(true)
  item.labelDetailsSupport(true)
  item.deprecatedSupport(true)
  item.commitCharactersSupport(true)
  item.tagSupport({"valueSet", {1}})
  item.resolveSupport({properties = {"documentation", "detail", "additionalTextEdits"}})
  return capabilities
end
local function setup_diagnostics()
  return vim.diagnostic.config({virtual_text = true, signs = true, underline = true, update_in_insert = false, severity_sort = false})
end
local function define_signs()
  sign_define("LspDiagnosticsSignError", {text = "\239\129\151 ", texthl = "LspDiagnosticsSignError"})
  sign_define("LspDiagnosticsSignWarning", {text = "\239\129\177 ", texthl = "LspDiagnosticsSignWarning"})
  sign_define("LspDiagnosticsSignInformation", {text = "\239\129\154 ", texthl = "LspDiagnosticsSignInformation"})
  return sign_define("LspDiagnosticsSignHint", {text = "\239\160\180", texthl = "LspDiagnosticsSignHint"})
end
local function format_async(err, result, ctx)
  if (not err and result) then
    local bufnr = ctx.bufnr
    if not vim.api.nvim_buf_get_option(bufnr, "modified") then
      local view = vim.fn.winsaveview()
      vim.lsp.util.apply_text_edits(result, bufnr, "utf-16")
      vim.fn.winrestview(view)
      if (bufnr == vim.api.nvim_get_current_buf()) then
        return vim.cmd("noautocmd :update")
      else
        return nil
      end
    else
      return nil
    end
  else
    return nil
  end
end
vim.lsp.handlers["textDocument/formatting"] = format_async
local capabilities
do
  local cmp0 = require("cmp_nvim_lsp")
  capabilities = cmp0.update_capabilities(vim.lsp.protocol.make_client_capabilities())
end
local function disable_formatting(client)
  local caps = client.server_capabilities
  caps.document_formatting = false
  caps.document_range_formatting = false
  caps.documentFormattingProvider = false
  return nil
end
local function setup_clangd()
  local caps = vim.lsp.protocol.make_client_capabilities()
  caps.offsetEncoding = {"utf-16"}
  return lspconfig.clangd.setup({cmd = {"clangd", "--background-index", "--compile-commands-dir", "build"}, init_options = {compilationDatabasePath = "build"}, root_dir = util.root_pattern("build/compile_commands.json"), capabilities = caps, on_attach = disable_formatting})
end
local function setup_jdtls()
  return lspconfig.jdtls.setup({root_dir = util.root_pattern(".git", "pom.xml"), on_attach = disable_formatting})
end
local function setup_rust_analyzer()
  return lspconfig.rust_analyzer.setup({settings = {["rust-analyzer"] = {checkOnSave = {enabled = true, command = "clippy"}}}})
end
local function setup_sumneko_lua()
  lspconfig.sumneko_lua.setup = lua_dev.setup
  return nil
end
local function setup_texlab()
  return lspconfig.texlab.setup({on_attach = disable_formatting})
end
local function setup_pyright()
  return lspconfig.pyright.setup({root_dir = util.root_pattern(".git")})
end
local function setup_servers()
  lspinstaller.setup({ensure_installed = {"clangd", "jdtls", "rust_analyzer", "sumneko_lua", "texlab", "pyright", "cmake", "clojure_lsp"}})
  setup_clangd()
  setup_jdtls()
  setup_rust_analyzer()
  setup_sumneko_lua()
  setup_texlab()
  setup_pyright()
  lspconfig.clojure_lsp.setup({})
  return lspconfig.cmake.setup({})
end
local function setup()
  setup_servers()
  setup_mappings()
  setup_diagnostics()
  return define_signs()
end
return {setup = setup}