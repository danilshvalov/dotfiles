local utils = require("floppy.utils")
local nmap, vmap = utils.nmap, utils.vmap
local lazy = utils.lazy

local lspsaga = require("lspsaga")
local diagnostic = require("lspsaga.diagnostic")
local code_action = require("lspsaga.codeaction")
local action = require("lspsaga.action")
local provider = require("lspsaga.provider")

local setup_mappings = function()
    nmap("<F2>", require("lspsaga.rename").rename)
    nmap("<Leader>d", provider.preview_definition)
    nmap("<Leader>s", require("lspsaga.signaturehelp").signature_help)
    nmap("<C-a>", code_action.code_action)
    vmap("<C-a>", code_action.range_code_action)
    nmap("<Leader>h", require("lspsaga.hover").render_hover_doc)
    nmap("[e", diagnostic.navigate("next"))
    nmap("]e", diagnostic.navigate("prev"))
    nmap("<C-h>", diagnostic.show_line_diagnostics)
    nmap("<C-f>", lazy(action.smart_scroll_with_saga, 1))
    nmap("<C-b>", lazy(action.smart_scroll_with_saga, -1))
end

local M = {}

M.setup = function()
    lspsaga.setup({
        rename_prompt_prefix = "",
        code_action_prompt = {
            enable = false,
        },
    })

    setup_mappings()
end

return M
