-- :fennel:1651697998
local npairs = require("nvim-autopairs")
local cond = require("nvim-autopairs.conds")
local ts_conds = require("nvim-autopairs.ts-conds")
local Rule = require("nvim-autopairs.rule")
local ignore_regexp = "[%w%.\208\144-\208\175%\208\176-\209\143\208\129\209\145%(%[%{%'%\"]"
npairs.setup({enable_check_bracket_line = false, fast_wrap = {map = "<A-x>", pattern = string.gsub("[%'%\"%)%>%]%)%}%%s%^]", "%s+", "")}, ignored_next_char = string.gsub(ignore_regexp, "%s+", ""), disable_filetype = {}, enable_moveright = false})
npairs.add_rule(Rule("/*", "*/"))
npairs.add_rule(Rule("r#\"", "\"#", "rust"))
npairs.add_rule(Rule("\\{", "\\}"))
npairs.add_rule(Rule("\\(", "\\)"))
npairs.add_rule(Rule("\\[", "\\]"))
local function setup()
end
return {setup = setup}