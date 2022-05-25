local null_ls = require("null-ls")
local api = vim.api

local utils = require("vdk.core.utils")
local map, new_cmd = utils.map, utils.new_cmd

local builtins = null_ls.builtins
local formatting = builtins.formatting
local diagnostics = builtins.diagnostics

local enable_format = true
local toggle_format = function()
    enable_format = not enable_format
end

new_cmd("ToggleFormat", toggle_format)
map("<Leader>tf", toggle_format)

local make_sources = function()
    return {
        builtins.code_actions.gitsigns,
        diagnostics.eslint,
        diagnostics.codespell,
        formatting.stylua,
        -- formatting.lua_format.with({
        --     extra_args = { "--spaces_inside_table_braces", "--extra-sep-at-table-end", "--chop-down-kv-table", "--chop-down-table" }
        -- }),
        formatting.prettier.with({ filetypes = { "java", "markdown" } }),
        formatting.prettierd.with({ filetypes = { "json", "javascript", "yaml" } }),
        formatting.clang_format,
        formatting.black,
        formatting.latexindent.with({ extra_args = { "-g /dev/null" } }),
        formatting.taplo,
        formatting.fnlfmt,
    }
end

local lsp_formatting = function(_)
    vim.lsp.buf.format({ async = true })
end

local format_file = function()
    if enable_format then
        lsp_formatting()
    end
end

local group_id = api.nvim_create_augroup("LspFormatting", {})

local on_attach = function(client, bufnr)
    if client.supports_method("textDocument/formatting") then
        map("<F3>", lsp_formatting)
        api.nvim_clear_autocmds({ group = group_id, buffer = bufnr })
        api.nvim_create_autocmd({ "BufWritePost" }, {
            group = group_id,
            callback = format_file,
            buffer = bufnr,
        })
    end
end

null_ls.setup({ sources = make_sources(), on_attach = on_attach })
