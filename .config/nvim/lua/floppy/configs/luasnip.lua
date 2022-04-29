local ls = require("luasnip")
local s = ls.snippet
local sn = ls.snippet_node
local isn = ls.indent_snippet_node
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node
local r = ls.restore_node
local events = require("luasnip.util.events")

ls.config.set_config({
    history = true,
    -- Update more often, :h events for more info.
    updateevents = "TextChanged,TextChangedI,InsertLeave",
    region_check_events = "CursorMoved",
    delete_check_events = "TextChanged,InsertLeave",
    -- treesitter-hl has 100, use something higher (default is 200).
    -- ext_base_prio = 300,
    -- minimal increase in priority.
    -- ext_prio_increase = 1,
    enable_autosnippets = true,
    store_selection_keys = true,
})

local function load_snippets()
    require("luasnip.loaders.from_snipmate").load()
    -- require("luasnip.loaders.from_vscode").load()
    -- ls.autosnippets = {
    --     tex = {
    --         s("->", t("\\to")),
    --         s("=>", t("\\to")),
    --     },
    -- }
    -- ls.autosnippets = {
    --     all = {
    --         s("\\(", { t({ "\\(" }), i(1), t({ "\\)" }), i(0) }),
    --         s("\\[", { t({ "\\[" }), i(1), t({ "\\]" }), i(0) }),
    --         s("\\Big(", { t({ "\\Big(" }), i(1), t({ "\\Big)" }), i(0) }),
    --         s("(", { t({ "(" }), i(1), t({ ")" }), i(0) }),
    --         s("{", { t({ "{" }), i(1), t({ "}" }), i(0) }),
    --         s("[", { t({ "[" }), i(1), t({ "]" }), i(0) }),
    --     },
    -- }
end

ls.cleanup()
load_snippets()

function _G.reload_snippets()
    ls.cleanup()
    load_snippets()
end

vim.api.nvim_command("command! ReloadSnippets lua reload_snippets()<CR>")

vim.api.nvim_command("autocmd BufWritePost *.snippets lua reload_snippets()")
