local treesitter = require("nvim-treesitter.configs")

-- TODO: remove
-- (set vim.o.foldmethod :expr)
-- (set vim.o.foldlevel 20)
-- (set vim.o.foldexpr "nvim_treesitter#foldexpr()")

treesitter.setup({
    ensure_installed = {
        "cpp", "python", "json", "rust", "java", "lua", "html", "latex", "norg",
    },
    highlight = {
        enable = true,
        disable = { "html", "latex" }
    },
    indent = {
        enable = false
    },
    autopairs = {
        enable = true
    }
})
