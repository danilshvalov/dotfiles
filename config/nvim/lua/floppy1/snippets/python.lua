-- :fennel:1652349914
local ls = require("luasnip")
local _local_1_ = require("luasnip.extras.fmt")
local fmt = _local_1_["fmt"]
local _local_2_ = ls
local snip = _local_2_["snippet"]
local node = _local_2_["snippet_node"]
local isn = _local_2_["indent_snippet_node"]
local text = _local_2_["text_node"]
local ins = _local_2_["insert_node"]
local func = _local_2_["function_node"]
local choice = _local_2_["choice_node"]
local dyn = _local_2_["dynamic_node"]
local restore = _local_2_["restore_node"]
local function _3_(_241, _242)
  return ("print" .. "(" .. (_242.captures)[1] .. ")")
end
local function _4_(_241, _242)
  return ("print" .. "(" .. (_242.captures)[1] .. ")")
end
ls.add_snippets("python", {snip({hidden = true, regTrig = true, trig = "([\"'].+[\"'])%.print"}, func(_3_, {})), snip({hidden = true, regTrig = true, trig = "(%S+)%.print"}, func(_4_, {}))})
local function _5_(_241, _242)
  return ("len" .. "(" .. (_242.captures)[1] .. ")")
end
local function _6_(_241, _242)
  return ("len" .. "(" .. (_242.captures)[1] .. ")")
end
ls.add_snippets("python", {snip({hidden = true, regTrig = true, trig = "([\"'].+[\"'])%.len"}, func(_5_, {})), snip({hidden = true, regTrig = true, trig = "(%S+)%.len"}, func(_6_, {}))})
return ls.add_snippets("python", {snip("init", {text("def __init__(self"), ins(1), text({"):", "\9"})}), snip("for", {text("for "), ins(1), text(" in "), ins(2), text({":", "\9"})}), snip("forr", {text("for "), ins(1), text(" in range("), ins(2), text({"):", "\9"})}), snip("cls", {text("class "), ins(1), text({":", "\9"}), text("def __init__(self"), ins(2), text({"):", "\9\9"})})})