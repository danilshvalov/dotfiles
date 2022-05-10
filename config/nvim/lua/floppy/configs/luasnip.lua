-- :fennel:1652084756
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
local function section_snips(prefix, name)
  local sections = {{trig = "sec", name = "\\section", prefix = "sec"}, {trig = "secs", name = "\\section*", prefix = "sec"}, {trig = "sub", name = "\\subsection", prefix = "sub"}, {trig = "subs", name = "\\subsection*", prefix = "sub"}, {trig = "ssub", name = "\\subsubsection", prefix = "ssub"}, {trig = "ssubs", name = "\\subsubsection*", prefix = "ssub"}}
  local tbl_15_auto = {}
  local i_16_auto = #tbl_15_auto
  for _, sec in pairs(sections) do
    local val_17_auto
    do
      local _let_4_ = sec
      local trig = _let_4_["trig"]
      local name0 = _let_4_["name"]
      local prefix0 = _let_4_["prefix"]
      val_17_auto = snip(trig, {text((name0 .. "{")), ins(1), text({"}", ("\\label{" .. prefix0 .. ":")}), rep(1), text({"}", ""})})
    end
    if (nil ~= val_17_auto) then
      i_16_auto = (i_16_auto + 1)
      do end (tbl_15_auto)[i_16_auto] = val_17_auto
    else
    end
  end
  return tbl_15_auto
end
local function setup_snippets()
  ls.add_snippets("all", {snip("--", text("\226\128\148"))})
  local function _6_(args, s)
    return (s.captures[1] .. s.captures[2] .. " ")
  end
  local function _7_(args, s)
    return (" " .. s.captures[1])
  end
  ls.add_snippets("tex", {snip({trig = "(\\[%B%b]ig?%g)([%(%[%{])", regTrig = true, hidden = true}, {func(_6_, {}), ins(1), func(_7_, {})}), snip("\\beg", fmt("\n                    \\begin{{{}}}\n                      {}\n                    \\end{{{}}}\n                    ", {ins(1), ins(0), rep(1)})), snip({trig = "mk", wordTrig = true}, fmt("\\({}\\)", {ins(1)})), snip("\\fr", {text("\\frac{"), ins(1), text("}{"), ins(2), text("}")}), snip("\\dfr", {text("\\dfrac{"), ins(1), text("}{"), ins(2), text("}")}), snip("\\lim", {text("\\lim_{"), ins(1), text("}{"), ins(2), text("}")}), snip("\\int", {text("\\int\\limits_{"), ins(1), text("}^{"), ins(2), text("}")}), snip("\\iint", {text("\\iint\\limits_{"), ins(1), text("}")}), snip("\\bo", {text("\\boxed{"), ins(1), text("}")})})
  ls.add_snippets("fennel", {snip("req", fmt("(local {} (require :{}))", {ins(1), ins(2)})), snip("im", fmt("(import-macros {{:{}}} :{})", {ins(1), ins(2)}))})
  return ls.add_snippets("cpp", {snip("main", fmt("\n                      int main() {{\n                      \9{}\n                      }}\n                      ", {ins(0)}))})
end
local function reload_snippets()
  ls.cleanup()
  load_snips({paths = (vim.fn.stdpath("config") .. "/lua/floppy/snippets")})
  return setup_snippets()
end
local function setup()
  ls.config.setup({update_events = "InsertLeave,TextChanged,TextChangedI", region_check_events = "CursorMoved,CursorHold,InsertEnter"})
  load_snips({paths = (vim.fn.stdpath("config") .. "/lua/floppy/snippets")})
  setup_snippets()
  return vim.api.nvim_create_user_command("ReloadSnippets", reload_snippets, {force = true})
end
return {setup = setup}