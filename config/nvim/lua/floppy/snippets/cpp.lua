-- :fennel:1652207921
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
return ls.add_snippets("cpp", {snip("inc", {text("#include <"), ins(1), text(">")}), snip("std", text("using namespace std;")), snip("cout", {text("std::cout << "), ins(1), text(" << std::endl")}), snip("main", {text({"int main() {", "\9"}), ins(0), text({"", "}"})})})