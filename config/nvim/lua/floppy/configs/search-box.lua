local map = vim.api.nvim_set_keymap
local opts = { silent = true, noremap = true }

map("", "<C-f>", "<Cmd>SearchBoxMatchAll<CR>", opts)
map("", "<C-A-f>", "<Cmd>SearchBoxReplace confirm=menu<CR>", opts)
map("", "<Leader>cs", "<Cmd>noh | SearchBoxClear<CR>", opts)

require("searchbox").setup({
    popup = {
        relative = "win",
        position = { row = "5%", col = "95%" },
        size = 30,
        border = {
            style = "rounded",
            highlight = "FloatBorder",
            text = { top = " Search ", top_align = "left" },
        },
        win_options = { winhighlight = "Normal:Normal" },
    },
})
