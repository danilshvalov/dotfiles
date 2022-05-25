-- :fennel:1652905954
local telescope = require("telescope")
local builtin = require("telescope.builtin")
local actions = require("telescope.actions")
local previewers = require("telescope.previewers")
local _2_
do
  local _1_ = {hidden = true, no_ignore = true}
  local function _3_(...)
    return builtin.find_files(_1_, ...)
  end
  _2_ = _3_
end
vim.keymap.set("n", "<Leader>ff", _2_, {silent = true})
vim.keymap.set("n", "<Leader>fr", builtin.oldfiles, {silent = true})
vim.keymap.set("n", "<Leader>fg", builtin.live_grep, {silent = true})
vim.keymap.set("n", "<Leader>fb", builtin.buffers, {silent = true})
vim.keymap.set("n", "<Leader>fh", builtin.help_tags, {silent = true})
vim.keymap.set("n", "<Leader>ls", builtin.lsp_document_symbols, {silent = true})
vim.keymap.set("n", "<Leader>gc", builtin.git_commits, {silent = true})
vim.keymap.set("n", "<Leader>gf", builtin.git_files, {silent = true})
vim.keymap.set("n", "<Leader>gb", builtin.git_branches, {silent = true})
vim.keymap.set("n", "<Leader>b", builtin.buffers, {silent = true})
vim.keymap.set("n", "z=", builtin.spell_suggest, {silent = true})
vim.keymap.set("n", "gr", builtin.lsp_references, {silent = true})
vim.keymap.set("n", "gd", builtin.lsp_definitions, {silent = true})
vim.keymap.set("n", "gi", builtin.lsp_implementations, {silent = true})
local default = {defaults = {prompt_prefix = " ", selection_caret = " ", entry_prefix = " ", initial_mode = "insert", selection_strategy = "reset", sorting_strategy = "ascending", layout_strategy = "vertical", layout_config = {vertical = {height = 0.6, prompt_position = "top", width = 0.8, mirror = true, preview_height = 0.4}}, use_less = true, preview = false, file_ignore_patterns = {"node_modules/.*", ".git/*"}, file_previewer = previewers.vim_buffer_cat.new, grep_previewer = previewers.vim_buffer_vimgrep.new, qflist_previewer = previewers.vim_buffer_qflist.new, buffer_previewer_maker = previewers.buffer_previewer_maker, mappings = {i = {["<C-Down>"] = actions.cycle_history_next, ["<C-Up>"] = actions.cycle_history_prev}}, history = {path = "~/.local/share/nvim/databases/telescope_history.sqlite3", limit = 100}}, extensions = {fzf = {fuzzy = true, override_generic_sorter = true, override_file_sorter = true, case_mode = "smart_case"}}}
local function setup()
  telescope.setup(default)
  telescope.load_extension("fzf")
  return true
end
return {setup = setup}