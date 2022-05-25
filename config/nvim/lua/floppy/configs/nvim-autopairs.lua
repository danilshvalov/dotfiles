-- :fennel:1652881067
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
local function _2_(...)
  local _1_ = Rule("{", "};")
  local function _3_(opts)
    local struct = string.match(opts.line, "struct%s*%S*%s*$")
    local class = string.match(opts.line, "class%s*%S*%s*$")
    return ((struct ~= nil) or (class ~= nil))
  end
  _1_:with_pair(_3_)
  return _1_
end
npairs.add_rule(_2_(...))
local function _5_(...)
  local _4_ = Rule("<", ">")
  local function _6_(_241)
    return (string.match(_241.line, "[%w%d]+$") ~= nil)
  end
  _4_:with_pair(_6_)
  return _4_
end
npairs.add_rule(_5_(...))
local function setup()
end
return {setup = setup}