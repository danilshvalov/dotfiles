local npairs = require("nvim-autopairs")
local Rule = require("nvim-autopairs.rule")

local ignore_regexp = "[%w%.А-Я%а-яЁё%(%[%{%'%\"]"

npairs.setup({
    enable_check_bracket_line = false,
    fast_wrap = {
        map = "<A-x>",
        pattern = string.gsub("[%'%\"%)%>%]%)%}%%s%^]", "%s+", "")
    },
    ignored_next_char = string.gsub(ignore_regexp, "%s+", ""),
    disable_filetype = {},
    enable_moveright = false,
})

npairs.add_rule(Rule("/*", "*/"))
npairs.add_rule(Rule("r#\"", "\"#", "rust"))
npairs.add_rule(Rule("\\{", "\\}"))
npairs.add_rule(Rule("\\(", "\\)"))
npairs.add_rule(Rule("\\[", "\\]"))


npairs.add_rule(
    Rule("{", "};")
    :with_pair(function(opts)
        local struct = string.match(opts.line, "struct%s*%S*%s*$")
        local class = string.match(opts.line, "class%s*%S*%s*$")
        return struct ~= nil or class ~= nil
    end)
)

npairs.add_rule(
    Rule("<", ">")
    :with_pair(function(opts)
        return string.match(opts.line, "[%w%d]+$") ~= nil
    end)
)
