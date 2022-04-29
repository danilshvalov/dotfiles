require("toggleterm").setup({
    -- open_mapping = "<C-t>",
    direction = "float",
    shade_terminals = false,
    start_in_insert = false,
})

local opts = { silent = true, noremap = true }
local map = vim.api.nvim_set_keymap
-- map("t", "<Esc><Esc>", "<C-\\><C-n>", opts)
-- map("t", "<C-t>", "<C-\\><C-n><Cmd>ToggleTerm<CR>", opts)
-- map("", "<Leader>m", "<Cmd>TermExec cmd='make'<CR>i", opts)
