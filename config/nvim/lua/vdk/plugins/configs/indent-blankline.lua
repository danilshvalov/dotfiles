local indentline = require("indent_blankline")

indentline.setup({
    show_first_indent_level = true,
    show_trailing_blankline_indent = false,
    filetype_exclude = {
        "help",
        "terminal",
        "alpha",
        "packer",
        "lspinfo",
        "TelescopePrompt",
        "TelescopeResults",
        "nvchad_cheatsheet",
        "lsp-installer",
        "",
    },
})
