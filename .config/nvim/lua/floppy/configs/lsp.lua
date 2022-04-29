local utils = require("floppy.utils")
local nmap = utils.nmap
local lsp = vim.lsp
local sign_define = vim.fn.sign_define

nmap("gD", lsp.buf.declaration)
nmap("<Leader>e", vim.diagnostic.open_float)

local make_capabilities = function()
    local capabilities = require("cmp_nvim_lsp").update_capabilities(lsp.protocol.make_client_capabilities())
    capabilities.textDocument.completion.completionItem.snippetSupport = true
    capabilities.textDocument.completion.completionItem.preselectSupport = true
    capabilities.textDocument.completion.completionItem.insertReplaceSupport = true
    capabilities.textDocument.completion.completionItem.labelDetailsSupport = true
    capabilities.textDocument.completion.completionItem.deprecatedSupport = true
    capabilities.textDocument.completion.completionItem.commitCharactersSupport = true
    capabilities.textDocument.completion.completionItem.tagSupport = { valueSet = { 1 } }
    capabilities.textDocument.completion.completionItem.resolveSupport = {
        properties = {
            "documentation",
            "detail",
            "additionalTextEdits",
        },
    }
    capabilities.offsetEncoding = { "utf-8" }

    return capabilities
end

local lspconfig = require("lspconfig")
lspconfig.tsserver.setup({
    capabilities = make_capabilities(),
    init_options = require("nvim-lsp-ts-utils").init_options,
    on_attach = function(client, bufnr)
        local function buf_set_option(...)
            vim.api.nvim_buf_set_option(bufnr, ...)
        end
        buf_set_option("omnifunc", "v:lua.vim.lsp.omnifunc")
    end,
})

vim.diagnostic.config({
    virtual_text = true,
    signs = true,
    underline = true,
    update_in_insert = false,
    severity_sort = false,
})

vim.api.nvim_command("command! ToggleVirtualText lua toggle_virtual_text()")

sign_define("LspDiagnosticsSignError", { text = " ", texthl = "LspDiagnosticsSignError" })
sign_define("LspDiagnosticsSignWarning", { text = " ", texthl = "LspDiagnosticsSignWarning" })
sign_define("LspDiagnosticsSignInformation", { text = " ", texthl = "LspDiagnosticsSignInformation" })
sign_define("LspDiagnosticsSignHint", { text = "", texthl = "LspDiagnosticsSignHint" })
