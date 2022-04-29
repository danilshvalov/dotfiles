local M = {}

M.setup = function()
    require("virt-column").setup({
        virtcolumn = "79,119",
        char = "┃",
        colorcolumn = "ColorColumn",
    })
end

return M
