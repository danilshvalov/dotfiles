local M = {}

M.setup = function()
    require("indent_blankline").setup({
        show_first_indent_level = false,
        show_trailing_blankline_indent = false,
        filetype_exclude = { "alpha", "NvimTree", "neo-tree" },
    })
end

return M
