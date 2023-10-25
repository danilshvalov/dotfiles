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

return {
  s(
    "src",
    fmt(
      [[
        #+begin_src {}
        {}
        #+end_src
      ]],
      { i(1), i(0) }
    )
  ),
  s("todo", t("*** TODO ")),
}
