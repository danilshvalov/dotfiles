require'hop'.setup()

vim.api.nvim_set_keymap('n', 'f', "<cmd>:HopChar1<CR>", {})
vim.api.nvim_set_keymap('n', '<C-f>', "<cmd>HopPattern<CR>", {})
