-- global config
vim.g.neoformat_run_all_formatters = 1

-- python
vim.g.neoformat_python_black = {
    exe = "black",
    args = { "-l 79", "-q", "-" },
    stdin = 1,
}
vim.g.neoformat_enabled_python = { "autopep8", "black" }

vim.g.neoformat_cpp_clangformat = {
    exe = "clang-format",
    args = { "-style=chromium" },
    stdin = 1,
}

vim.g.neoformat_enabled_cpp = { "clangformat" }
vim.g.neoformat_enabled_java = { "prettier" }

vim.g.neoformat_enabled_typescript = { "prettier" }
vim.g.neoformat_enabled_typescriptreact = { "prettier" }

vim.g.neoformat_enabled_lua = { "stylua" }

-- format on save
-- vim.api.nvim_exec(
-- [[
-- augroup fmt
-- autocmd!
-- autocmd BufWritePre * silent :Neoformat
-- augroup END
-- ]],
-- true
-- )
