local telescope = require("telescope")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")
local previewers = require("telescope.previewers")

local nmap = require("vdk.core.utils").nmap

nmap("<Leader>ff", function()
    builtin.find_files({ hidden = true, no_ignore = true })
end, { desc = "Find files" })
nmap("<Leader>fr", builtin.oldfiles, { desc = "Oldfiles" })
nmap("<Leader>fg", builtin.live_grep, { desc = "Live grep" })
nmap("<Leader>fb", builtin.buffers, { desc = "Buffers" })
nmap("<Leader>fh", builtin.help_tags, { desc = "Help tags" })
nmap("<Leader>ls", builtin.lsp_document_symbols, { desc = "Document symbols" })
nmap("z=", builtin.spell_suggest, { desc = "Spell suggest" })
nmap("gr", builtin.lsp_references, { desc = "LSP references" })
nmap("gd", builtin.lsp_definitions, { desc = "LSP definitions" })
nmap("gi", builtin.lsp_implementations, { desc = "LSP implementations" })

telescope.setup({
    defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        entry_prefix = " ",
        initial_mode = "insert",
        selection_strategy = "reset",
        sorting_strategy = "ascending",
        layout_strategy = "vertical",
        layout_config = {
            vertical = {
                height = 0.6,
                prompt_position = "top",
                width = 0.8,
                mirror = true,
                preview_height = 0.4,
            },
        },
        use_less = true,
        preview = false,
        file_ignore_patterns = { "node_modules/.*", ".git/*" },
        file_previewer = previewers.vim_buffer_cat.new,
        grep_previewer = previewers.vim_buffer_vimgrep.new,
        qflist_previewer = previewers.vim_buffer_qflist.new,
        buffer_previewer_maker = previewers.buffer_previewer_maker,
        mappings = {
            i = {
                ["<C-Down>"] = actions.cycle_history_next,
                ["<C-Up>"] = actions.cycle_history_prev,
            },
        },
    },
    extensions = {
        fzf = {
            fuzzy = true,
            foverride_generic_sorter = true,
            foverride_file_sorter = true,
            fcase_mode = "smart_case",
        },
    },
})

telescope.load_extension("fzf")
