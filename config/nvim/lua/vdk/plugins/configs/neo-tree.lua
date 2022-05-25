local neotree = require("neo-tree")
local command = require("neo-tree.command")

local utils = require("vdk.core.utils")
local nmap, hi = utils.nmap, utils.hi

nmap("<C-t>", function()
	command.execute({ toggle = true, position = "right", source = "filesystem" })
end)
hi("NeoTreeDirectoryIcon", { bg = "NONE" })
hi("NeoTreeTitleBar", { fg = "black", bg = "white" })

neotree.setup({
	filesystem = {
		window = { width = 30, position = "right" },
		filtered_items = { hide_dotfiles = false },
	},
	event_handlers = {
		{
			event = "file_opened",
			handler = function()
				neotree.close_all()
			end,
		},
	},
	default_component_configs = {
		indent = {
			indent_size = 2,
			padding = 0,
			with_markers = true,
			indent_marker = "│",
			last_indent_marker = "└",
			highlight = "NeoTreeIndentMarker",
			with_expanders = true,
			expander_collapsed = "",
			expander_expanded = "",
			expander_highlight = "NeoTreeExpander",
		},
	},
})
