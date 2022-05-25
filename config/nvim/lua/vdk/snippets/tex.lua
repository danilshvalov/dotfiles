local ls = require("luasnip")
local s = ls.snippet
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local rep = require("luasnip.extras").rep
local fmt = require("luasnip.extras.fmt").fmt

local lfmt = function(format, ...)
    return fmt(format, { ... }, { delimiters = "<>" })
end

local env_snip = function(trig, name)
    local format_str = [[
        \begin{<>}
            <>
        \end{<>}
    ]]
    return s(trig, lfmt(format_str, name, i(0), name))
end

local sec_snip = function(trig, name, prefix)
    local format_str = [[
        \<>{<>}
        \label{<>:<>}
    ]]
    return s(trig, lfmt(format_str, name, i(1), prefix, rep(1)))
end

local wrap_snip = function(trig, name)
    return s({ trig = trig, wordTrig = false }, lfmt([[\<>{<>}]], name, i(1)))
end

local text_snip = function(trig, text)
    return s(trig, t(text))
end

local beg_snip = function()
    local format_str = [[
        \begin{<>}
            <>
        \end{<>}
    ]]
    return s("beg", lfmt(format_str, i(1), i(0), rep(1)))
end

local img_snip = function()
    local format_str = [[
        \begin{center}
            \includegraphics[width=<>\textwidth]{<>}
        \end{center}
    ]]
    return s("img", lfmt(format_str, i(1), i(0)))
end

local fig_snip = function()
    local format_str = [[
        \begin{figure}[H]
            \centering
            \includegraphics[width=<>\textwidth]{<>}
            \caption{<>}
            \label{fig:<>}
        \end{figure}
    ]]
    return s("fig", lfmt(format_str, i(1), i(2), i(3), rep(3)))
end

ls.add_snippets("tex", {
    beg_snip(),
    img_snip(),
    fig_snip(),

    s({ trig = "(\\[%B%b]ig?%g)([%(%[%{])", regTrig = true, hidden = true }, {
        f(function(_, snip)
            return snip.captures[1] .. snip.captures[2] .. " "
        end, {}),
        i(1),
        f(function(_, snip)
            return " " .. snip.captures[1]
        end, {}),
    }),

    env_snip("eq", "equation"),
    env_snip("eqs", "equation*"),
    env_snip("item", "itemize"),
    env_snip("enum", "enumerate"),
    env_snip("side", "siderules"),
    env_snip("the", "theorem"),
    env_snip("thes", "theorem*"),
    env_snip("pro", "proof"),
    env_snip("prop", "property"),
    env_snip("gat", "gather"),
    env_snip("gats", "gather*"),
    env_snip("gatd", "gathered"),
    env_snip("cas", "dcases*"),
    env_snip("cen", "center"),
    env_snip("long", "longtable"),

    sec_snip("sec", "section", "sec"),
    sec_snip("secs", "section*", "sec"),
    sec_snip("sub", "subsection", "sub"),
    sec_snip("subs", "subsection*", "sub"),
    sec_snip("ssub", "subsubsection", "ssub"),
    sec_snip("ssubs", "subsubsection*", "ssub"),

    text_snip("\\a", "\\alpha"),
    text_snip("\\b", "\\beta"),
    text_snip("\\g", "\\gamma"),
    text_snip("\\l", "\\lambda"),
    text_snip("\\p", "\\partial"),
    text_snip("\\D", "\\Delta"),
    text_snip("\\i", "\\item "),
    text_snip("\\vep", "\\varepsilon"),
    text_snip("\\vphi", "\\varphi"),
    text_snip("->", "\\to"),
    text_snip("=>", "\\implies"),
    text_snip("\\inf", "\\infty"),
    text_snip("<=", "\\leq"),
    text_snip(">=", "\\geq"),
    text_snip("!=", "\\neq"),
    text_snip("?=", "\\approx"),
    text_snip("===", "\\equiv"),
    text_snip("*", "\\cdot"),
    text_snip("\\q", "\\quad"),
    text_snip("\\qq", "\\qquad"),
    text_snip("\\qqq", "\\qquad \\qquad"),

    s("mk", fmt("\\({}\\)", { i(1) })),
    s("\\fr", lfmt("\\frac{<>}{<>}", i(1), i(2))),
    s("\\dfr", lfmt("\\dfrac{<>}{<>}", i(1), i(2))),
    s("\\lim", lfmt("\\lim_{<>}{<>}", i(1), i(2))),
    s("\\int", lfmt("\\int\\limits_{<>}{<>}", i(1), i(2))),
    s("\\iint", lfmt("\\iint\\limits_{<>}", i(1))),
    s("\\iiint", lfmt("\\iiint\\limits_{<>}", i(1))),

    wrap_snip("\\t", "text"),
    wrap_snip("\\s", "sqrt"),
    wrap_snip("\\bf", "textbf"),
    wrap_snip("\\it", "textit"),
    wrap_snip("\\tt", "texttt"),
    wrap_snip("\\bo", "boxed"),
})
