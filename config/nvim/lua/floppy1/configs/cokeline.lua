local get_hex = require("cokeline/utils").get_hex
local map = vim.api.nvim_set_keymap
local plugin_opts = { silent = true }
local opts = { silent = true, noremap = true }

-- map('n', '<A-Left>', '<Plug>(cokeline-focus-prev)', plugin_opts)
-- map('n', '<A-Right>', '<Plug>(cokeline-focus-next)', plugin_opts)
map("n", "<S-A-Left>", "<Plug>(cokeline-switch-prev)", plugin_opts)
map("n", "<S-A-Right>", "<Plug>(cokeline-switch-next)", plugin_opts)
map("n", "<Leader>q", "<Cmd>bd<CR>", opts)

require("cokeline").setup({
    default_hl = {
        focused = { fg = get_hex("Normal", "fg"), bg = "NONE" },
        unfocused = { fg = get_hex("Comment", "fg"), bg = "NONE" },
    },

    components = {
        {
            text = function(buffer)
                return (buffer.index ~= 1) and "▏" or ""
            end,
            hl = { fg = get_hex("Normal", "fg") },
        },
        {
            text = function(buffer)
                return "    " .. buffer.devicon.icon
            end,
            hl = {
                fg = function(buffer)
                    return buffer.devicon.color
                end,
            },
        },
        {
            text = function(buffer)
                return buffer.filename .. "    "
            end,
            hl = {
                style = function(buffer)
                    return buffer.is_focused and "bold" or nil
                end,
            },
        },
        { text = "", delete_buffer_on_left_click = true },
        { text = "  " },
    },
})
