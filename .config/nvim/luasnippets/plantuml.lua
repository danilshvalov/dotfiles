local ls = require("luasnip")
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local fmta = require("luasnip.extras.fmt").fmta

return {
  s(
    "en",
    fmta(
      [[
        entity {
            <>
        }
      ]],
      { i(0) }
    )
  ),
  s("col", fmta("column(<>)", { i(1) })),
  s("om", t("||--o{")),
}
