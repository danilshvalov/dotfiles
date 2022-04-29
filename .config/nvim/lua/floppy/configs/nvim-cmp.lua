-- :fennel:1651221107
local cmp = require("cmp")
local snippy = require("snippy")
local api = vim.api
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
local function setup_snippets()
  local function _1_(args)
    return snippy.expand_snippet(args.body)
  end
  return {expand = _1_}
end
local function setup_mappings()
  local function _2_(fallback)
    if snippy.can_expand_or_advance() then
      return snippy.expand_or_advance()
    elseif cmp.visible() then
      return cmp.select_next_item()
    else
      return fallback()
    end
  end
  local function _4_(fallback)
    if cmp.visible() then
      return cmp.select_prev_item()
    elseif snippy.can_jump(-1) then
      return snippy.previous()
    else
      return fallback()
    end
  end
  return {["<C-d>"] = map_scroll_docs(-4), ["<C-f>"] = map_scroll_docs(4), ["<C-Space>"] = cmp.mapping(cmp.mapping.complete(), {"i", "c"}), ["<Up>"] = cmp.mapping(cmp.mapping.select_prev_item({behavior = cmp.SelectBehavior.Select}), {"i"}), ["<Down>"] = cmp.mapping(cmp.mapping.select_next_item({behavior = cmp.SelectBehavior.Select}), {"i"}), ["<C-y>"] = cmp.config.disable, ["<C-e>"] = cmp.mapping({i = cmp.mapping.abort(), c = cmp.mapping.close()}), ["<CR>"] = cmp.mapping.confirm({select = false}), ["<Tab>"] = cmp.mapping(_2_, {"i", "c", "s"}), ["<S-Tab>"] = cmp.mapping(_4_, {"i", "c", "s"})}
end
local function setup_sources()
  local sources = {"path", "orgmode", "nvim_lsp", "nvim_lsp_signature_help", "snippy"}
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
  return cmp.setup({snippet = setup_snippets(), mapping = setup_mappings(), sources = setup_sources()})
end
return {setup = setup}