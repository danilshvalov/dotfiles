local function set_keymap(...) vim.api.nvim_set_keymap(...) end
local opts = {noremap = true, silent = true}

set_keymap('n', 'gg', '<Cmd>LazyGit<CR>', opts)

