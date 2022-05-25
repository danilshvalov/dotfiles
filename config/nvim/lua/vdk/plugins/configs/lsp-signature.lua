local signature = require("lsp_signature")

signature.setup({
    bind = true,
    doc_lines = 10,
    floating_window = true,
    floating_window_above_cur_line = true,
    floating_window_off_x = 0,
    floating_window_off_y = 0,
    fix_pos = false,
    hint_enable = false,
    max_height = 12,
    max_width = 80,
    handler_opts = { border = "rounded" },
    always_trigger = false,
})
