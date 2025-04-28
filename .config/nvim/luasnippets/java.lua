local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local extras = require("luasnip.extras")
local f = ls.function_node
local l = extras.lambda
local rep = extras.rep
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local postfix = require("luasnip.extras.postfix").postfix

local to_camel_case = function(args)
  local arg = args[1][1]
  vim.print(args)
  return string.lower(arg:sub(1, 1)) .. arg:sub(2)
end

return {
  s("sout", fmt("System.out.println({});", { i(1) })),
  s(
    "gs",
    fmta(
      [[
        public <> get<>() {
          return <>;
        }

        public void set<>(<> <>) {
          this.<> = <>;
        }
      ]],
      {
        i(1),
        i(2),
        f(to_camel_case, { 2 }),
        rep(2),
        rep(1),
        f(to_camel_case, { 2 }),
        f(to_camel_case, { 2 }),
        f(to_camel_case, { 2 }),
      }
    )
  ),
}
