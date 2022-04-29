local leap = require("leap")
local utils = require("floppy.utils.mappings")
local nmap, nunmap = utils.nmap, utils.nunmap
local cmd = vim.cmd

local M = {}

local setup_highlights = function()
    cmd("hi LeapMatch guifg=#ff768e")
    cmd("hi LeapLabelPrimary guibg=#ff768e")
    cmd("hi LeapLabelSecondary guibg=#ff768e")
    cmd("hi LeapBackdrop guifg=gray")
end

M.setup = function()
    leap.setup({})
    leap.set_default_keymaps()

    setup_highlights()
end

return M
