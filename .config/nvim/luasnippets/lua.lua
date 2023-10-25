local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local fmta = require("luasnip.extras.fmt").fmta
local postfix = require("luasnip.extras.postfix").postfix

return {
  s(
    "fn",
    fmta(
      [[
        function(<>)
          <>
        end
      ]],
      { i(1), i(0) }
    )
  ),
  s(
    "fnn",
    fmta(
      [[
        function <>(<>)
          <>
        end
      ]],
      { i(1), i(2), i(0) }
    )
  ),
}
