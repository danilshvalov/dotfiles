local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local fmt = require("luasnip.extras.fmt").fmt
local fmta = require("luasnip.extras.fmt").fmta
local postfix = require("luasnip.extras.postfix").postfix

return {
  s(
    "table",
    fmt(
      [[
        CREATE TABLE IF NOT EXISTS {}.{} (
            {}
        );
      ]],
      { i(1), i(2), i(0) }
    )
  ),
  s("id", t("id TEXT PRIMARY KEY")),
  s("fk", fmt("FOREIGN KEY ({}) REFERENCES {} ({})", { i(1), i(2), i(3) })),
  s("nn", t("NOT NULL")),
  s("pk", t("PRIMARY KEY")),
}
