--[[ require("indent_blankline").setup({
    buftype_exclude = { "terminal" },
    filetype_exclude = { "alpha" },
    show_first_indent_level = false,
    show_current_context = true,
})

vim.api.nvim_command("highlight IndentBlanklineContextChar guifg=#bbbbbb") ]]

vim.g.indentLine_char = "│"
vim.g.indentLine_fileTypeExclude = { "alpha", "NvimTree", "help", "Fm" }
