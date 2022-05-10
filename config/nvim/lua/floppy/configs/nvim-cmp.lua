-- :fennel:1652082775
local cmp = require("cmp")
local luasnip = require("luasnip")
local api = vim.api
local icons = {Text = "\239\131\137", Method = "\239\154\166", Function = "\239\158\148", Constructor = "\226\140\152", Field = "\239\176\160", Variable = "\239\156\155", Class = "\239\180\175", Interface = "\239\131\168", Module = "\239\163\146", Property = "\239\176\160", Unit = "\239\165\172", Value = "\239\162\159", Enum = "\239\133\157", Keyword = "\239\168\139", Snippet = "\239\153\176", Color = "\239\163\151", File = "\239\156\152", Reference = "\239\156\134", Folder = "\239\129\187", EnumMember = "\239\133\157", Constant = "\239\163\191", Struct = "\239\173\132", Event = "\239\131\167", Operator = "\239\154\148", TypeParameter = ""}
local function is_word(line, col)
  local lines = api.nvim_buf_get_lines(0, (line - 1), line, true)
  local current_line = lines[1]
  local char = current_line:sub(0, 0)
  return not char:match("%s")
end
local function has_words_before()
  local line, col = unpack(api.nvim_win_get_cursor(0))
  return ((col ~= 0) and is_word(line, col))
end
local function map_scroll_docs(n)
  return cmp.mapping(cmp.mapping.scroll_docs(n), {"i", "c"})
end
local function setup_mappings()
  local function _1_(fallback)
    if luasnip.expand_or_jumpable() then
      return luasnip.expand_or_jump()
    elseif cmp.visible() then
      return cmp.select_next_item()
    else
      return fallback()
    end
  end
  local function _3_(fallback)
    if cmp.visible() then
      return cmp.select_prev_item()
    elseif luasnip.jumpable(-1) then
      return luasnip.jump(-1)
    else
      return fallback()
    end
  end
  return {["<C-d>"] = map_scroll_docs(-4), ["<C-f>"] = map_scroll_docs(4), ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), {"i", "c"}), ["<C-n>"] = cmp.mapping(cmp.mapping.complete(), {"i", "c"}), ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item({behavior = cmp.SelectBehavior.Select}), {"i"}), ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item({behavior = cmp.SelectBehavior.Select}), {"i"}), ["<C-y>"] = cmp.config.disable, ["<C-e>"] = cmp.mapping({i = cmp.mapping.abort(), c = cmp.mapping.close()}), ["<CR>"] = cmp.mapping.confirm({select = false}), ["<Tab>"] = cmp.mapping(_1_, {"i", "c", "s"}), ["<S-Tab>"] = cmp.mapping(_3_, {"i", "c", "s"})}
end
local function setup_sources()
  local sources = {"path", "orgmode", "nvim_lsp", "nvim_lsp_signature_help", "neorg", "luasnip"}
  local tbl_15_auto = {}
  local i_16_auto = #tbl_15_auto
  for index, value in ipairs(sources) do
    local val_17_auto = {name = value, priority = index}
    if (nil ~= val_17_auto) then
      i_16_auto = (i_16_auto + 1)
      do end (tbl_15_auto)[i_16_auto] = val_17_auto
    else
    end
  end
  return tbl_15_auto
end
local function setup()
  cmp.setup.cmdline(":", {sources = cmp.config.sources({{name = "path"}}, {{name = "cmdline"}})})
  local function _6_(args)
    return luasnip.lsp_expand(args.body)
  end
  local function _7_(_, vim_item)
    vim_item.menu = vim_item.kind
    vim_item.kind = icons[vim_item.kind]
    return vim_item
  end
  return cmp.setup({snippet = {expand = _6_}, mapping = setup_mappings(), sources = setup_sources(), formatting = {fields = {"kind", "abbr", "menu"}, format = _7_}})
end
return {setup = setup}