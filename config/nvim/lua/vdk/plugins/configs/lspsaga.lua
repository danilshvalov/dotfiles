local lspsaga = require("lspsaga")
local diagnostic = require("lspsaga.diagnostic")
local code_action = require("lspsaga.codeaction")
local action = require("lspsaga.action")
local provider = require("lspsaga.provider")
local rename = require("lspsaga.rename").rename
local signature_help = require("lspsaga.signaturehelp").signature_help
local render_hover_doc = require("lspsaga.hover").render_hover_doc

local utils = require("vdk.core.utils")
local nmap, vmap = utils.nmap, utils.vmap

nmap("<Leader>r", rename)
nmap("<Leader>d", provider.preview_definition)
nmap("<Leader>s", signature_help)
nmap("<C-a>", code_action.code_action)
vmap("<C-a>", code_action.range_code_action)
nmap("<Leader>h", render_hover_doc)
nmap("[e", diagnostic.navigate("next"))
nmap("]e", diagnostic.navigate("prev"))
nmap("<C-h>", diagnostic.show_line_diagnostics)
nmap("<C-f>", function()
	action.smart_scroll_with_saga(1)
end)
nmap("<C-b>", function()
	action.smart_scroll_with_saga(-1)
end)

lspsaga.setup({
	rename_prompt_prefix = "",
	code_action_prompt = {
		enable = false,
	},
})
