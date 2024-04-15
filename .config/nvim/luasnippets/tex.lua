local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local extras = require("luasnip.extras")
local l = extras.lambda
local rep = extras.rep
local fmta = require("luasnip.extras.fmt").fmta
local postfix = require("luasnip.extras.postfix").postfix

local function text(trig, value)
  return s({ trig = trig, wordTrig = true }, t(value))
end

local function const(trig, value)
  return text(trig, "\\" .. value)
end

local function wrap_with(trig, lhs, rhs, exit)
  return s({ trig = trig, wordTrig = true }, { t(lhs), i(exit and 0 or 1), t(rhs) })
end

local function cmd(trig, value)
  return wrap_with(trig, "\\" .. value .. "{", "}")
end

local function env(trig, env_name, params)
  return wrap_with(
    trig,
    { "\\begin{" .. env_name .. "}" .. (params or ""), "\t" },
    { "", "\\end{" .. env_name .. "}" },
    true
  )
end

local function list_env(trig, env_name, params)
  return wrap_with(
    trig,
    { "\\begin{" .. env_name .. "}" .. (params or ""), "\t\\item " },
    { "", "\\end{" .. env_name .. "}" },
    true
  )
end

local function pf(trig, ...)
  return postfix({ trig = trig, match_pattern = "[%S]+$" }, { ... })
end

local function pf_wrap(trig, value)
  return pf(trig, l("\\" .. value .. "{" .. l.POSTFIX_MATCH .. "}"))
end

local function frac(trig, value)
  return s({
    trig = trig,
    wordTrig = false,
  }, fmta("\\" .. value .. "{<>}{<>}", { i(1), i(2) }))
end

return {
  s(
    "beg",
    fmta(
      [[
        \begin{<>}
          <>
        \end{<>}
      ]],
      { i(1), i(0), rep(1) }
    )
  ),
  s(
    "frame",
    fmta(
      [[
        \begin{frame}
          \frametitle{<>}

          <>
        \end{frame}
      ]],
      { i(1), i(0) }
    )
  ),
  s(
    "minted",
    fmta(
      [[
        \begin{minted}{<>}
          <>
        \end{minted}
      ]],
      { i(1, "text"), i(0) }
    )
  ),
  s(
    "fig",
    fmta(
      [[
        \begin{figure}[H]
          \centering
          \includegraphics[width=<>\textwidth]{<>}
          \caption{<>}
          \label{fig:<>}
        \end{figure}
      ]],
      { i(1), i(2), i(3), i(0) }
    )
  ),
  wrap_with("mk", "\\(", "\\)"),
  wrap_with("dm", { "\\[", "\t" }, { "", "\\]" }, true),
  cmd("bf", "textbf"),
  cmd("it", "textit"),
  cmd("tt", "texttt"),
  cmd("bb", "mathbb"),
  cmd("te", "text"),
  cmd("se", "section"),
  cmd("ses", "section*"),
  cmd("ssu", "subsubsection"),
  cmd("ssus", "subsubsection*"),
  cmd("su", "subsection"),
  cmd("sus", "subsection*"),
  cmd("use", "usepackage"),
  const("qu", "quad"),
  const("qq", "qquad"),
  env("cas", "dcases*"),
  env("side", "siderules"),
  env("gs", "gather*"),
  env("eq", "equation"),
  env("ex", "example"),
  env("exs", "example*"),
  env("th", "theorem"),
  env("ths", "theorem*"),
  env("pm", "pmatrix"),
  const("\\i", "item "),
  const("al", "alpha"),
  const("be", "beta"),
  const("ga", "gamma"),
  const("la", "lambda"),
  const("pa", "partial"),
  const("ve", "varepsilon"),
  const("vp", "varphi"),
  const("De", "Delta"),
  const("om", "omega"),
  const("Om", "Omega"),
  list_env("item", "itemize"),
  list_env("enum", "enumerate"),
  list_env("enumc", "enumerate", "[wide, labelwidth=!, labelindent=0pt]"),
  frac("fr", "frac"),
  frac("dfr", "dfrac"),
  frac("un", "underset"),
  text("..", "\\ldots"),
  s("*", t("\\cdot")),
  text("->", "\\to"),
  text("<=", "\\leq"),
  text(">=", "\\geq"),
  text("!=", "\\neq"),
  text("=>", "\\implies"),
  pf_wrap(".bf", "textbf"),
  pf_wrap(".it", "textit"),
  s("fl", fmta("\\foreignlanguage{english}{<>}", { i(1) })),
}
