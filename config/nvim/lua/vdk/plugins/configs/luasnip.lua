local ls = require("luasnip")
local lua_load = require("luasnip.loaders.from_lua").load

local new_cmd = require("vdk.core.utils").new_cmd

local load_snippets = function()
    lua_load({ paths = vim.fn.stdpath("config") .. "/lua/vdk/snippets" })
end

new_cmd("ReloadSnippets", function()
    ls.cleanup()
    load_snippets()
end)

ls.config.setup({
    update_events = "InsertLeave,TextChanged,TextChangedI",
    region_check_events = "CursorMoved,CursorHold,InsertEnter",
})

load_snippets()
