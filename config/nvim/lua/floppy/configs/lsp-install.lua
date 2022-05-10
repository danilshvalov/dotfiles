-- :fennel:1651221107
local lsp_installer = require("nvim-lsp-installer")
local util = require("lspconfig/util")
local capabilities
do
  local cmp = require("cmp_nvim_lsp")
  capabilities = cmp.update_capabilities(vim.lsp.protocol.make_client_capabilities())
end
local function disable_formatting(client)
  local caps = client.resolved_capabilities
  caps.document_formatting = false
  caps.document_range_formatting = false
  return nil
end
local function setup_clangd(opts)
  opts.cmd = {"clangd", "--background-index", "--compile-commands-dir", "build"}
  opts.init_options = {compilationDatabasePath = "build"}
  opts.root_dir = util.root_pattern("build/compile_commands.json")
  opts.capabilities.offsetEncoding = {"utf-16"}
  opts.on_attach = disable_formatting
  return opts
end
local function setup_jdtls(opts)
  opts.root_dir = util.root_pattern(".git", "pom.xml")
  opts.on_attach = disable_formatting
  return opts
end
local function setup_rust_analyzer(opts)
  opts.settings({["rust-analyzer"] = {checkOnSave = {enabled = true, command = "clippy"}}})
  return opts
end
local function setup_sumneko_lua(opts)
  local lua_dev = require("lua-dev")
  return lua_dev.setup()
end
local function setup_texlab(opts)
  opts.on_attach = disable_formatting
  return opts
end
local function setup_server(server)
  local opts = {capabilities = capabilities}
  if (server.name == "clangd") then
    opts = setup_clangd(opts)
  else
  end
  if (server.name == "jdtls") then
    opts = setup_jdtls(opts)
  else
  end
  if (server.name == "rust_analyzer") then
    opts = setup_rust_analyzer(opts)
  else
  end
  if (server.name == "sumneko_lua") then
    opts = setup_sumneko_lua(opts)
  else
  end
  if (server.name == "texlab") then
    opts = setup_texlab(opts)
  else
  end
  return server:setup(opts)
end
local function setup()
  return lsp_installer.on_server_ready(setup_server)
end
return {setup = setup}