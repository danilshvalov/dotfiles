local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local fmta = require("luasnip.extras.fmt").fmta

return {
  s(
    "table",
    fmta(
      [[
        table(<>) {
            primary_key(id): UUID
            --
            <>
        }
      ]],
      { i(1), i(0) }
    )
  ),
  s("col", fmta("column(<>): <>", { i(1), i(0) })),
  s("pk", fmta("primary_key(<>): <>", { i(1), i(0) })),
  s("fk", fmta("foreign_key(<>): <>", { i(1), i(0) })),
}
