local lualine = require("lualine")

local line_sections = {
    lualine_a = {
        {
            function()
                return " "
            end,
            padding = { right = 0 },
        },
    },
    lualine_b = { "mode" },
    lualine_c = { { "branch", icon = "" }, "diff", "diagnostics" },
    lualine_x = {
        "progress",
        {
            function()
                return vim.opt.tabstop:get()
            end,
            icon = "הּ",
        },
        { "encoding", fmt = string.upper },
        { "filetype", icons_enabled = false },
    },
    lualine_y = {
        "location",
    },
    lualine_z = {
        {
            function()
                return " "
            end,
            padding = { left = 0 },
        },
    },
}

local winbar_sections = {
    lualine_a = {},
    lualine_b = {},
    lualine_c = {
        "%=",
        {
            "filename",
            file_status = true,
            path = 3,
        },
    },
    lualine_x = {},
    lualine_y = {},
    lualine_z = {},
}

vim.o.showmode = false

lualine.setup({
    options = {
        theme = "auto",
        globalstatus = true,
        component_separators = "",
        section_separators = "",
    },
    winbar = winbar_sections,
    inactive_winbar = winbar_sections,
    sections = line_sections,
    inactive_item = line_sections,
})
