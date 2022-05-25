local alpha = require("alpha")
local dashboard = require("alpha.themes.dashboard")
dashboard.section.header.val = {
    [[                                                                       ]],
    [[                                                                       ]],
    [[                                                                     ]],
    [[       ███████████           █████      ██                     ]],
    [[      ███████████             █████                             ]],
    [[      ████████████████ ███████████ ███   ███████     ]],
    [[     ████████████████ ████████████ █████ ██████████████   ]],
    [[    █████████████████████████████ █████ █████ ████ █████   ]],
    [[  ██████████████████████████████████ █████ █████ ████ █████  ]],
    [[ ██████  ███ █████████████████ ████ █████ █████ ████ ██████ ]],
    [[ ██████   ██  ███████████████   ██ █████████████████ ]],
    [[ ██████   ██  ███████████████   ██ █████████████████ ]],
}

dashboard.section.buttons.val = {
    dashboard.button("e", "  New file", "<Cmd>ene <BAR> startinsert <CR>"),
    dashboard.button("o", "  Browse old files", "<Cmd>Telescope oldfiles<CR>"),
    dashboard.button("f", "  Find files", "<Cmd>Telescope find_files<CR>"),
    dashboard.button("g", "  Grep string", "<Cmd>Telescope live_grep<CR>"),
    dashboard.button("s", "  Sessions", "<Cmd>Telescope sessions<CR>"),
    dashboard.button("q", "  Quit NVIM", "<Cmd>qa<CR>"),
}
alpha.setup(dashboard.opts)
