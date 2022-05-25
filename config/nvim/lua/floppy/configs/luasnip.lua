-- :fennel:1653327198
local ls = require("luasnip")
local snip = ls.snippet
local node = ls.snippet_node
local isn = ls.indent_snippet_node
local text = ls.text_node
local ins = ls.insert_node
local func = ls.function_node
local choice = ls.choice_node
local dyn = ls.dynamic_node
local restore = ls.restore_node
local _local_1_ = require("luasnip.extras")
local rep = _local_1_["rep"]
local events = require("luasnip.util.events")
local ai = require("luasnip.nodes.absolute_indexer")
local _local_2_ = require("luasnip.extras.fmt")
local fmt = _local_2_["fmt"]
local util = require("luasnip.util.util")
local node_util = require("luasnip.nodes.util")
local _local_3_ = require("luasnip.loaders.from_lua")
local load_snips = _local_3_["load"]
local function add_snippets(...)
  return ls.add_snippets("tex", {...})
end
local function text_snip(trig, name)
  return snip(trig, text(name))
end
local function setup_snippets()
  ls.add_snippets("all", {snip("--", text("\226\128\148"))})
  local function _4_(args, s)
    return (s.captures[1] .. s.captures[2] .. " ")
  end
  local function _5_(args, s)
    return (" " .. s.captures[1])
  end
  ls.add_snippets("tex", {snip({trig = "(\\[%B%b]ig?%g)([%(%[%{])", regTrig = true, hidden = true}, {func(_4_, {}), ins(1), func(_5_, {})})})
  return ls.add_snippets("fennel", {snip("req", fmt("(local {} (require :{}))", {ins(1), ins(2)})), snip("im", fmt("(import-macros {{:{}}} :{})", {ins(1), ins(2)})), snip("fn", fmt("(fn {} [] {})", {ins(1), ins(0)}))})
end
local function reload_snippets()
  ls.cleanup()
  load_snips({paths = (vim.fn.stdpath("config") .. "/lua/vdk/snippets")})
  return setup_snippets()
end
local function setup()
  ls.config.setup({update_events = "InsertLeave,TextChanged,TextChangedI", region_check_events = "CursorMoved,CursorHold,InsertEnter"})
  load_snips({paths = (vim.fn.stdpath("config") .. "/lua/vdk/snippets")})
  setup_snippets()
  return vim.api.nvim_create_user_command("ReloadSnippets", reload_snippets, {force = true})
end
return {setup = setup}