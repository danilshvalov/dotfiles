local neogen = require("neogen")
local map = require("vdk.core.utils").map

neogen.setup({})

map("<Leader>gd", neogen.generate)
