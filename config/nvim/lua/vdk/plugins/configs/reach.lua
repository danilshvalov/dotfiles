local reach = require("reach")

local nmap = require("vdk.core.utils").nmap

nmap("<C-X>", function()
	reach.buffers({ handle = "dynamic" })
end)

nmap("<Leader>q", "<Cmd>Bdelete<CR>")

reach.setup({ notifications = false })
