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
  s("in", fmt("#include <{}>", { i(1) })),
  s("inl", fmt('#include "{}"', { i(1) })),
  s("cout", fmt("std::cout << {} << std::endl;", { i(0) })),
  s("once", t("#pragma once")),
  s(
    "ns",
    fmta(
      [[
        namespace <> {

        <>

        } // <>
      ]],
      { i(1), i(0), rep(1) }
    )
  ),
  s(
    "main",
    fmta(
      [[
        int main() {
          <>
        }
      ]],
      { i(0) }
    )
  ),
  postfix(".beg", {
    f(function(_, parent)
      local match = parent.snippet.env.POSTFIX_MATCH
      return string.format("%s.begin(), %s.end()", match, match)
    end, {}),
  }),
  s(
    "ser",
    fmta(
      [[
      userver::formats::json::Value Serialize(
        const <>& data,
        userver::formats::serialize::To<<userver::formats::json::Value>> /* tag */
      ) {
        userver::formats::json::ValueBuilder builder;
        builder["<>"] = data.<>;<>
        return builder.ExtractValue();
      }
      ]],
      { i(1), i(2), i(3), i(0) }
    )
  ),
  s(
    "parse",
    fmta(
      [[
      <> Parse(
        const userver::formats::json::Value& json,
        userver::formats::parse::To<<<>>> /* tag */
      ) {
        return <>{
          json["<>"].As<<<>>>(),<>
        };
      }
      ]],
      { i(1), rep(1), rep(1), i(2), i(3), i(0) }
    )
  ),
}
