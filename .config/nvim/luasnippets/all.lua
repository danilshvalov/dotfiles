local ls = require("luasnip")
local s = ls.snippet
local f = ls.function_node
local i = ls.insert_node
local t = ls.text_node
local fmt = require("luasnip.extras.fmt").fmt

local function comment(trig, value)
  return s(
    trig,
    f(function()
      return vim.bo.commentstring:gsub("%%s", value)
    end)
  )
end

return {
  comment("todo", "TODO: "),
  comment("fix", "FIXME: "),
  comment("hack", "HACK: "),
  s("--", t("—")),
  s("qu", fmt("«{}»", { i(1) })),
}
