local lspinstaller = require("nvim-lsp-installer")
local lspconfig = require("lspconfig")
local util = require("lspconfig/util")
local lsp = vim.lsp
local sign_define = vim.fn.sign_define

local nmap = require("vdk.core.utils").nmap

sign_define("LspDiagnosticsSignError", { text = " ", texthl = "LspDiagnosticsSignError" })
sign_define("LspDiagnosticsSignWarning", { text = " ", texthl = "LspDiagnosticsSignWarning" })
sign_define("LspDiagnosticsSignInformation", { text = " ", texthl = "LspDiagnosticsSignInformation" })
sign_define("LspDiagnosticsSignHint", { text = " ", texthl = "LspDiagnosticsSignHint" })

nmap("gD", lsp.buf.declaration)
nmap("<Leader>e", vim.diagnostic.open_float)

local format_async = function(err, result, ctx)
    if err ~= nil or result == nil then
        return
    end

    local bufnr = ctx.bufnr
    if not vim.api.nvim_buf_get_option(bufnr, "modified") then
        local view = vim.fn.winsaveview()
        vim.lsp.util.apply_text_edits(result, bufnr, "utf-8")
        vim.fn.winrestview(view)
        if bufnr == vim.api.nvim_get_current_buf() then
            vim.api.nvim_command("noautocmd :update")
        end
    end
end
vim.lsp.handlers["textDocument/formatting"] = format_async

local disable_format = function(client)
    local caps = client.server_capabilities
    caps.document_formatting = false
    caps.document_range_formatting = false
    caps.documentFormattingProvider = false
end

lspinstaller.setup({
    ensure_installed = {
        "clangd",
        "jdtls",
        "rust_analyzer",
        "sumneko_lua",
        "texlab",
        "pyright",
        "cmake",
    },
})

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false,
})

local caps = vim.lsp.protocol.make_client_capabilities()
caps.offsetEncoding = { "utf-16" }
lspconfig.clangd.setup({
    cmd = { "clangd", "--background-index", "--compile-commands-dir", "build" },
    init_options = { compilationDatabasePath = "build" },
    root_dir = util.root_pattern("build/compile_commands.json"),
    capabilities = caps,
    on_attach = disable_format,
})

lspconfig.sumneko_lua.setup({
    settings = {
        Lua = {
            diagnostics = {
                globals = { "vim" },
            },
        },
    },
})
lspconfig.texlab.setup({ on_attach = disable_format })
lspconfig.clojure_lsp.setup({})
lspconfig.cmake.setup({})
lspconfig.jdtls.setup({
    root_dir = util.root_pattern(".git", "pom.xml"),
    on_attach = disable_format,
})
lspconfig.pyright.setup({
    root_dir = util.root_pattern(".git"),
})

lspconfig.rust_analyzer.setup({
    settings = {
        ["rust-analyzer"] = {
            checkOnSave = {
                enabled = true,
                command = "clippy",
            },
        },
    },
})
