local cmp = require("cmp")
local luasnip = require("luasnip")

local icons = {
    Text = "",
    Method = "",
    Function = "",
    Constructor = "⌘",
    Field = "ﰠ",
    Variable = "",
    Class = "ﴯ",
    Interface = "",
    Module = "",
    Property = "ﰠ",
    Unit = "塞",
    Value = "",
    Enum = "",
    Keyword = "廓",
    Snippet = "",
    Color = "",
    File = "",
    Reference = "",
    Folder = "",
    EnumMember = "",
    Constant = "",
    Struct = "פּ",
    Event = "",
    Operator = "",
    TypeParameter = "",
}

local mappings = {
    ["<C-n>"] = cmp.mapping(cmp.mapping.complete(), { "i", "c" }),
    ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item(), { "i", "c", "s" }),
    ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "c", "s" }),
    ["<C-e>"] = cmp.mapping({ i = cmp.mapping.abort(), c = cmp.mapping.close() }),
    ["<CR>"] = cmp.mapping.confirm({ select = false }),
    ["<Tab>"] = cmp.mapping({
        i = function(fallback)
            if cmp.get_selected_entry() then
                cmp.confirm()
            elseif luasnip.expand_or_jumpable() then
                luasnip.expand_or_jump()
            else
                fallback()
            end
        end,
        s = function(fallback)
            if luasnip.jumpable(1) then
                luasnip.jump(1)
            else
                fallback()
            end
        end,
        c = cmp.mapping.select_next_item(),
    }),
    ["<S-Tab>"] = cmp.mapping({
        i = function(fallback)
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end,
        s = function(fallback)
            if luasnip.jumpable(-1) then
                luasnip.jump(-1)
            else
                fallback()
            end
        end,
        c = cmp.mapping.select_prev_item(),
    }),
}

local sources = {
    "path",
    "orgmode",
    "nvim_lsp",
    "nvim_lsp_signature_help",
    "neorg",
    "luasnip",
}

local priorities = {}

for index, value in ipairs(sources) do
    priorities[index] = { name = value, priority = index }
end

cmp.setup({
    snippet = {
        expand = function(args)
            return luasnip.lsp_expand(args.body)
        end,
    },
    mapping = mappings,
    sources = priorities,
    formatting = {
        fields = { "kind", "abbr", "menu" },
        format = function(_, vim_item)
            vim_item.menu = vim_item.kind
            vim_item.kind = icons[vim_item.kind]
            return vim_item
        end,
    },
})

cmp.setup.cmdline(":", { sources = cmp.config.sources({ { name = "path" } }, { { name = "cmdline" } }) })
