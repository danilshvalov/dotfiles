local theme = require("github-theme")
local hi = require("vdk.core.utils").hi

theme.setup({
	theme_style = "dark_default",
})

vim.cmd("colorscheme github_dark_default")

hi("CmpItemKindSnippetDefault", { fg = "#666666" })
hi("cppTSProperty", { fg = "#e1e4e8" })
hi("cppTSType", { fg = "#75beff" })
hi("VertSplit", { fg = "#505050" })
